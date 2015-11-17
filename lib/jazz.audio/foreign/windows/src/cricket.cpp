#include <ck/ck.h>
#include <ck/config.h>
#include <ck/bank.h>
#include <ck/sound.h>

#include "cricket.h"


CkConfig config;


int CricketInit()
{
	return CkInit(&config);
}


void CricketShutdown()
{
	CkShutdown();
}


void CricketUpdate()
{
	CkUpdate();
}


void* CricketNewBankSound(char* path)
{
    CkBank* bank = CkBank::newBank(path, kCkPathType_FileSystem);
    CkSound* sound = CkSound::newBankSound(bank, 0);
    return (void*) sound;
}


void* CricketNewStreamSound(char* path)
{
    CkSound* sound = CkSound::newStreamSound(path, kCkPathType_FileSystem);
    return (void*) sound;
}


void CricketDestroy(void* sound)
{
	((CkSound*) sound)->destroy();
}


void CricketPlay(void* sound)
{
	((CkSound*) sound)->play();
}


void CricketStop(void* sound)
{
	((CkSound*) sound)->stop();
}


bool CricketPlaying(void* sound)
{
	return ((CkSound*) sound)->isPlaying();
}



void CricketPause(void* sound)
{
	((CkSound*) sound)->setPaused(true);
}


void CricketResume(void* sound)
{
	((CkSound*) sound)->setPaused(false);
}


bool CricketPaused(void* sound)
{
	return ((CkSound*) sound)->isPaused();
}


void CricketSetLoop(void* sound)
{
	((CkSound*) sound)->setLoop(0, -1);
	((CkSound*) sound)->setLoopCount(-1);
}


void CricketReleaseLoop(void* sound)
{
	((CkSound*) sound)->releaseLoop();
}


float CricketGetVolume(void* sound)
{
	return ((CkSound*) sound)->getVolume();
}


void CricketSetVolume(void* sound, float vol)
{
	((CkSound*) sound)->setVolume(vol);
}


void CricketSet3dEnabled(void* sound, bool flag)
{
	((CkSound*) sound)->set3dEnabled(flag);
}


void CricketSet3dPosition(void* sound, float x, float y, float z)
{
	((CkSound*) sound)->set3dPosition(x, y, z);
}


void CricketSet3dAttenuation(float nearDist, float farDist)
{
	CkSound::set3dAttenuation(kCkAttenuationMode_Linear, nearDist, farDist, 0);
}


void CricketSet3dListenerPosition(float eyeX, float eyeY, float eyeZ, float lookAtX, float lookAtY, float lookAtZ, float upX, float upY, float upZ)
{
	CkSound::set3dListenerPosition(eyeX, eyeY, eyeZ, lookAtX, lookAtY, lookAtZ, upX, upY, upZ);
}
