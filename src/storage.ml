open API
module XenAPI = Client.Client

exception Sr_type_unsupported

let _dd = "/bin/dd"
let _sparse_dd = Util.find_existing_file [
	"/opt/xensource/libexec/sparse_dd";
	"/usr/lib/xcp/lib/sparse_dd";
]

let get_vhd_path ~rpc ~session_id ~vdi =
	let sr = XenAPI.VDI.get_SR ~rpc ~session_id ~self:vdi in
	let sr_type = XenAPI.SR.get_type ~rpc ~session_id ~self:sr in
	match sr_type with
	| "nfs" | "ext" ->
		Printf.sprintf "/var/run/sr-mount/%s/%s.vhd"
			(XenAPI.SR.get_uuid ~rpc ~session_id ~self:sr)
			(XenAPI.VDI.get_uuid ~rpc ~session_id ~self:vdi)
	| "lvm" | "lvmohba" | "lvmoiscsi" ->
		Printf.sprintf "/dev/VG_XenStorage-%s/VHD-%s"
			(XenAPI.SR.get_uuid ~rpc ~session_id ~self:sr)
			(XenAPI.VDI.get_uuid ~rpc ~session_id ~self:vdi)
	| _ ->
		raise Sr_type_unsupported

let with_dom0_vbd ~rpc ~session_id ~vdi ~mode f =
	let vbd = XenAPI.VBD.create ~rpc ~session_id
		~vM:(Util.get_dom0 ~rpc ~session_id)
		~vDI:vdi
		~userdevice:"autodetect"
		~bootable:false
		~mode
		~_type:`Disk
		~unpluggable:true
		~empty:false
		~other_config:[]
		~qos_algorithm_type:""
		~qos_algorithm_params:[]
	in
	XenAPI.VBD.plug ~rpc ~session_id ~self:vbd;
	Util.finally
		(fun () ->
			let device = XenAPI.VBD.get_device ~rpc ~session_id ~self:vbd in
			f (Filename.concat "/dev" device))
		(fun () ->
			XenAPI.VBD.unplug ~rpc ~session_id ~self:vbd;
			XenAPI.VBD.destroy ~rpc ~session_id ~self:vbd)

let make_empty_dst_vdi ~rpc ~session_id ~src_vdi ~dst_sr =
	let src_vdi_rec = XenAPI.VDI.get_record ~rpc ~session_id ~self:src_vdi in
	let dst_vdi_rec = {
		src_vdi_rec with
		API.vDI_SR = dst_sr;
		API.vDI_sm_config = [];
	} in
	XenAPI.VDI.create_from_record ~rpc ~session_id ~value:dst_vdi_rec

let dd ~input_path ~output_path =
	ignore (Forkhelpers.execute_command_get_output
		_dd
		[
			(Printf.sprintf "if=%s" input_path);
			(Printf.sprintf "of=%s" output_path);
		])

let sparse_dd ~input_path ~output_path ~size =
	ignore (Forkhelpers.execute_command_get_output
		_sparse_dd
		[
			"-src"; input_path;
			"-dest"; output_path;
			"-size"; Int64.to_string size;
		])

let copy_data ~rpc ~session_id ~src_vdi ~dst_vdi ~dd_program ~mode =
	with_dom0_vbd ~rpc ~session_id ~vdi:src_vdi ~mode:`RO
		(fun src_dev ->
			with_dom0_vbd ~rpc ~session_id ~vdi:dst_vdi ~mode:`RW
				(fun dst_dev ->
					let input_path, output_path = match mode with
					| Config.Tapdevice -> src_dev, dst_dev
					| Config.Filesystem ->
						(get_vhd_path ~rpc ~session_id ~vdi:src_vdi),
						(get_vhd_path ~rpc ~session_id ~vdi:dst_vdi)
					in
					match dd_program with
					| Config.Dd -> dd ~input_path ~output_path
					| Config.Sparse_dd ->
						let size = XenAPI.VDI.get_virtual_size ~rpc ~session_id ~self:src_vdi in
						sparse_dd ~input_path ~output_path ~size))

let do_copy ~rpc ~session_id ~config =
	let src_vdi = config.Config.src_vdi in
	let dst_vdi = make_empty_dst_vdi ~rpc ~session_id
		~src_vdi ~dst_sr:config.Config.dst_sr in
	copy_data ~rpc ~session_id ~src_vdi ~dst_vdi
		~dd_program:config.Config.dd_program
		~mode:config.Config.mode
