open API
module XenAPI = Client.Client

exception Sr_type_unsupported

let _dd = "/bin/dd"
let _sparse_dd = "/opt/xensource/libexec/sparse_dd"

let get_vhd_path ~rpc ~session_id ~sr ~vdi =
	let sr_type = XenAPI.SR.get_type ~rpc ~session_id ~self:sr in
	match sr_type with
	| "nfs" | "ext" ->
		Printf.sprintf "/var/run/sr-mount/%s/%s.vhd"
			(XenAPI.SR.get_uuid ~rpc ~session_id ~self:sr)
			(XenAPI.VDI.get_uuid ~rpc ~session_id ~self:vdi)
	| "lvm" | "lvmohba" | "lvmoiscsi" ->
		Printf.sprintf "/dev/sm/backend/%s/%s"
			(XenAPI.SR.get_uuid ~rpc ~session_id ~self:sr)
			(XenAPI.VDI.get_uuid ~rpc ~session_id ~self:vdi)
	| _ ->
		raise Sr_type_unsupported

let with_dom0_vbd ~rpc ~session_id ~vdi ~mode ~f =
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
			f device)
		(fun () ->
			XenAPI.VBD.unplug ~rpc ~session_id ~self:vbd;
			XenAPI.VBD.destroy ~rpc ~session_id ~self:vbd)

let make_empty_dst_vdi ~rpc ~session_id ~src_vdi ~dst_sr =
	let src_vdi_rec = XenAPI.VDI.get_record ~rpc ~session_id ~self:src_vdi in
	let dst_vdi_rec = {
		src_vdi_rec with
		API.vDI_SR = dst_sr;
	} in
	XenAPI.VDI.create_from_record ~rpc ~session_id ~value:dst_vdi_rec
