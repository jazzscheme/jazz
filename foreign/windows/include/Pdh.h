#ifndef _PDH_H_
#define _PDH_H_

#include <windows.h>
#include <winperf.h>

typedef LONG            PDH_STATUS;
#define PDH_FUNCTION    PDH_STATUS __stdcall

#define PDH_MAX_COUNTER_PATH       1024

typedef HANDLE       PDH_HCOUNTER;
typedef HANDLE       PDH_HQUERY;
typedef HANDLE       PDH_HLOG;

typedef PDH_STATUS (__stdcall * CounterPathCallBack)(DWORD_PTR);


typedef struct _PDH_FMT_COUNTERVALUE {
    DWORD    CStatus;
    union {
        LONG        longValue;
        double      doubleValue;
        LONGLONG    largeValue;
        LPCSTR      AnsiStringValue;
        LPCWSTR     WideStringValue;
    };
} PDH_FMT_COUNTERVALUE, * PPDH_FMT_COUNTERVALUE;


typedef struct _BrowseDlgConfig_W {
    DWORD   bIncludeInstanceIndex:1,
            bSingleCounterPerAdd:1,
            bSingleCounterPerDialog:1,
            bLocalCountersOnly:1,
            bWildCardInstances:1,
            bHideDetailBox:1,
            bInitializePath:1,
            bDisableMachineSelection:1,
            bIncludeCostlyObjects:1,
            bShowObjectBrowser:1,
            bReserved:22;

    HWND                hWndOwner;
    LPWSTR              szDataSource;
    LPWSTR              szReturnPathBuffer;
    DWORD               cchReturnPathLength;
    CounterPathCallBack pCallBack;
    DWORD_PTR           dwCallBackArg;
    PDH_STATUS          CallBackStatus;
    DWORD               dwDefaultDetailLevel;
    LPWSTR              szDialogBoxCaption;
} PDH_BROWSE_DLG_CONFIG_W, * PPDH_BROWSE_DLG_CONFIG_W;


PDH_FUNCTION
PdhBrowseCountersW(
    PPDH_BROWSE_DLG_CONFIG_W pBrowseDlgData
);


PDH_FUNCTION
PdhOpenQueryW(
    LPCWSTR      szDataSource,
    DWORD_PTR    dwUserData,
    PDH_HQUERY * phQuery
);

PDH_FUNCTION
PdhAdd009CounterW(
    PDH_HQUERY     hQuery,
    LPCWSTR        szFullCounterPath,
    DWORD_PTR      dwUserData,
    PDH_HCOUNTER * phCounter
);

PDH_FUNCTION
PdhCloseQuery(
    PDH_HQUERY hQuery
);

PDH_FUNCTION
PdhCollectQueryData(
    PDH_HQUERY hQuery
);

PDH_FUNCTION
PdhGetFormattedCounterValue(
    PDH_HCOUNTER          hCounter,
    DWORD                 dwFormat,
    LPDWORD               lpdwType,
    PPDH_FMT_COUNTERVALUE pValue
);


#define PDH_BROWSE_DLG_CONFIG       PDH_BROWSE_DLG_CONFIG_W
#define PPDH_BROWSE_DLG_CONFIG      PPDH_BROWSE_DLG_CONFIG_W

#define PdhOpenQuery                PdhOpenQueryW
#define PdhBrowseCounters           PdhBrowseCountersW

#endif

