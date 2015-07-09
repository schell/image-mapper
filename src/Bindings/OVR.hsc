#include <bindings.dsl.h>
#if defined(_WIN32)
 #define OVR_OS_WIN32
#elif defined(__APPLE__)
 #define OVR_OS_MAC
#elif defined(__linux__)
 #define OVR_OS_LINUX
#endif

#include <LibOVR/OVR_Version.h>
#include <LibOVR/OVR_CAPI.h>
#include <LibOVR/OVR_CAPI_GL.h>

module Bindings.OVR where

import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

#ccall ovr_Initialize   , CInt -> IO CInt
