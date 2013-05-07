let xapi_rpc request =
	Rpc_client.do_rpc_unix
		~content_type:(Rpc_client.content_type_of_string "text/xml")
		~filename:(Filename.concat "/var/lib/xcp" "xapi")
		~path:"/" request

open API
module XenAPI = Client.Client

let () =
	let rpc = xapi_rpc in
	let session_id = XenAPI.Session.login_with_password
		~rpc ~uname:"dd-test" ~pwd:"" ~version:"1.1" in
	let config = Config.load ~rpc ~session_id in
	Storage.do_copy ~rpc ~session_id ~config
