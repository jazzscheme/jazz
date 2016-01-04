#ifdef __cplusplus
extern "C" {
#endif

__declspec(dllexport) int __cdecl CricketInit();
__declspec(dllexport) void __cdecl CricketShutdown();
__declspec(dllexport) void __cdecl CricketUpdate();
__declspec(dllexport) void* __cdecl CricketNewBankSound(char* path);
__declspec(dllexport) void* __cdecl CricketNewStreamSound(char* path);
__declspec(dllexport) void __cdecl CricketDestroy(void* sound);
__declspec(dllexport) bool __cdecl CricketReady(void* sound);
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
__declspec(dllexport) void __cdecl CricketSet3dEnabled(void* sound, bool flag);
__declspec(dllexport) void __cdecl CricketSet3dPosition(void* sound, float x, float y, float z);
__declspec(dllexport) void __cdecl CricketSet3dAttenuation(float nearDist, float farDist);
__declspec(dllexport) void __cdecl CricketSet3dListenerPosition(float eyeX, float eyeY, float eyeZ, float lookAtX, float lookAtY, float lookAtZ, float upX, float upY, float upZ);

#ifdef __cplusplus
}
#endif
