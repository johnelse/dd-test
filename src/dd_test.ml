let xapi_rpc request =
	Rpc_client.do_rpc_unix
		~content_type:(Rpc_client.content_type_of_string "text/xml")
		~filename:(Filename.concat "/var/lib/xcp" "xapi")
		~path:"/" request

open API
module XenAPI = Client.Client

let get_dom0 ~rpc ~session_id =
	Inventory.inventory_filename := "/etc/xensource-inventory";
	let control_domain_uuid = Inventory.lookup Inventory._control_domain_uuid in
	XenAPI.VM.get_by_uuid ~rpc ~session_id ~uuid:control_domain_uuid

let () =
	let rpc = xapi_rpc in
	let session_id = XenAPI.Session.login_with_password
		~rpc ~uname:"dd-test" ~pwd:"" ~version:"1.1" in
	let _ = Config.load ~rpc ~session_id in
	let dom0 = get_dom0 ~rpc ~session_id in
	print_endline (Ref.of_string dom0)
