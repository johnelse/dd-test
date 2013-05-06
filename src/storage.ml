module XenAPI = Client.Client

exception Sr_unsupported

let get_vhd_path ~rpc ~session_id ~sr ~vdi =
	let sr_type = XenAPI.SR.get_type ~rpc ~session_id ~self:sr in
	match sr_type with
	| "nfs" | "ext" ->
		Printf.sprintf "/var/run/sr-mount/%s/%s"
			(XenAPI.SR.get_uuid ~rpc ~session_id ~self:sr)
			(XenAPI.VDI.get_uuid ~rpc ~session_id ~self:vdi)
	| "lvm" | "lvmohba" | "lvmoiscsi" ->
		Printf.sprintf "/dev/sm/backend/%s/%s"
			(XenAPI.SR.get_uuid ~rpc ~session_id ~self:sr)
			(XenAPI.VDI.get_uuid ~rpc ~session_id ~self:vdi)
	| _ ->
		raise Sr_unsupported
