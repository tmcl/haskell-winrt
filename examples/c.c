#define UNICODE
#include <windows.h>


HRESULT query_interface(void * this, void * riid, void ** out) {
	MessageBox(NULL,  L"you query", L"you queried", MB_ICONINFORMATION);
	return 1;
}

HRESULT add_ref(void * this) {
	MessageBox(NULL,  L"you added ref", L"you added ref", MB_ICONINFORMATION);
	return 1;
}

HRESULT release(void * this) {
	MessageBox(NULL,  L"you released ref", L"you release ref", MB_ICONINFORMATION);
	return 1;
}

// HRESULT get_iids(void * this, void*iidCount, void**iids) {
// 	MessageBox(NULL,  L"you gat iids", L"you got iids", MB_ICONINFORMATION);
// 	return 1;
// }
// 
// HRESULT get_runtime_class_name(void * this, void* className) {
// 	MessageBox(NULL,  L"you gat classname", L"you got classname", MB_ICONINFORMATION);
// 	return 1;
// }
// 
// HRESULT get_trustlevel(void * this, void* trustlevel) {
// 	MessageBox(NULL,  L"i don't trust you", L"you got trustlevel", MB_ICONINFORMATION);
// 	return 1;
// }

HRESULT invoke(void * this, void* params) {
	MessageBox(NULL,  L"you gat invoke", L"you got invoking", MB_ICONINFORMATION);
	return 1;
}
