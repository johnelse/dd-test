open API
module XenAPI = Client.Client

exception Sr_type_unsupported

let _dd = "/bin/dd"
let _sparse_dd = Util.find_existing_file [
	"/opt/xensource/libexec/sparse_dd";
	"/usr/lib/xcp/lib/sparse_dd";
]

let get_vhd_path ~sr_type ~sr_uuid ~vdi_uuid =
	match sr_type with
	| "nfs" | "ext" ->
		Printf.sprintf "/var/run/sr-mount/%s/%s.vhd" sr_uuid vdi_uuid
	| "lvm" | "lvmohba" | "lvmoiscsi" ->
		Printf.sprintf "/dev/VG_XenStorage-%s/VHD-%s" sr_uuid vdi_uuid
	| _ ->
		raise Sr_type_unsupported

let with_activated_vhd ~rpc ~session_id ~vdi f =
	let sr = XenAPI.VDI.get_SR ~rpc ~session_id ~self:vdi in
	let sr_type = XenAPI.SR.get_type ~rpc ~session_id ~self:sr in
	let sr_uuid = XenAPI.SR.get_uuid ~rpc ~session_id ~self:sr in
	let vdi_uuid = XenAPI.VDI.get_uuid ~rpc ~session_id ~self:vdi in
	let vhd_path = get_vhd_path ~sr_type ~sr_uuid ~vdi_uuid in
	match sr_type with
	| "nfs" | "ext" ->
		f vhd_path
	| "lvm" | "lvmohba" | "lvmoiscsi" ->
		failwith "not implemented"
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

let with_prepared_vdi ~rpc ~session_id ~vdi ~mode ~vbd_mode f =
	match mode with
	| Config.Filesystem -> with_activated_vhd ~rpc ~session_id ~vdi f
	| Config.Tapdevice -> with_dom0_vbd ~rpc ~session_id ~vdi ~mode:vbd_mode f

let make_empty_dst_vdi ~rpc ~session_id ~src_vdi ~dst_sr =
	let src_vdi_rec = XenAPI.VDI.get_record ~rpc ~session_id ~self:src_vdi in
	let dst_vdi_rec = {
		src_vdi_rec with
		API.vDI_SR = dst_sr;
		API.vDI_sm_config = [];
	} in
	XenAPI.VDI.create_from_record ~rpc ~session_id ~value:dst_vdi_rec

let dd ~input_path ~output_path =
	let args = [
		(Printf.sprintf "if=%s" input_path);
		(Printf.sprintf "of=%s" output_path);
	] in
	Printf.printf "Calling %s with args [%s]\n"
		_dd (String.concat "; " args);
	let output = Forkhelpers.execute_command_get_output _dd args in
	Printf.printf "%s stdout: %s\n" _dd (fst output);
	Printf.printf "%s stderr: %s\n" _dd (snd output)

let sparse_dd ~input_path ~output_path ~size =
	let args = [
		"-src"; input_path;
		"-dest"; output_path;
		"-size"; Int64.to_string size;
	] in
	Printf.printf "Calling %s with args [%s]\n"
		_sparse_dd (String.concat "; " args);
	let output = Forkhelpers.execute_command_get_output _sparse_dd args in
	Printf.printf "%s stdout: %s\n" _sparse_dd (fst output);
	Printf.printf "%s stderr: %s\n" _sparse_dd (snd output)

let copy_data ~rpc ~session_id ~src_vdi ~dst_vdi ~dd_program ~mode =
	with_prepared_vdi ~rpc ~session_id ~vdi:src_vdi ~mode ~vbd_mode:`RO
		(fun input_path ->
			with_prepared_vdi ~rpc ~session_id ~vdi:dst_vdi ~mode ~vbd_mode:`RW
				(fun output_path ->
					Printf.printf "Copying from path %s to path %s\n" input_path output_path;
					match dd_program with
					| Config.Dd -> dd ~input_path ~output_path
					| Config.Sparse_dd ->
						let size = XenAPI.VDI.get_virtual_size ~rpc ~session_id ~self:src_vdi in
						sparse_dd ~input_path ~output_path ~size))

let do_copy ~rpc ~session_id ~config =
	let src_vdi = config.Config.src_vdi in
	let dst_vdi = make_empty_dst_vdi ~rpc ~session_id
		~src_vdi ~dst_sr:config.Config.dst_sr in
	Printf.printf "Will copy into VDI %s\n"
		(XenAPI.VDI.get_uuid ~rpc ~session_id ~self:dst_vdi);
	copy_data ~rpc ~session_id ~src_vdi ~dst_vdi
		~dd_program:config.Config.dd_program
		~mode:config.Config.mode
