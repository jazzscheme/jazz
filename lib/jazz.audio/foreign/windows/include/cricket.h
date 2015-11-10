#ifdef __cplusplus
extern "C" {
#endif

__declspec(dllexport) int __cdecl CricketInit();
__declspec(dllexport) void __cdecl CricketShutdown();
__declspec(dllexport) void __cdecl CricketUpdate();
__declspec(dllexport) void* __cdecl CricketNewBankSound(char* path);
__declspec(dllexport) void* __cdecl CricketNewStreamSound(char* path);
__declspec(dllexport) void __cdecl CricketDestroy(void* sound);
__declspec(dllexport) void __cdecl CricketPlay(void* sound);
__declspec(dllexport) void __cdecl CricketStop(void* sound);
__declspec(dllexport) bool __cdecl CricketPlaying(void* sound);
__declspec(dllexport) void __cdecl CricketPause(void* sound);
__declspec(dllexport) void __cdecl CricketResume(void* sound);
__declspec(dllexport) bool __cdecl CricketPaused(void* sound);
__declspec(dllexport) void __cdecl CricketSetLoop(void* sound);
__declspec(dllexport) void __cdecl CricketReleaseLoop(void* sound);
__declspec(dllexport) float __cdecl CricketGetVolume(void* sound);
__declspec(dllexport) void __cdecl CricketSetVolume(void* sound, float vol);

#ifdef __cplusplus
}
#endif
