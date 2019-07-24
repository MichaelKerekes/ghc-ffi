//////////////////////////////////////////////////////////////////////////////
//
//  Mana Runtime
//
//////////////////////////////////////////////////////////////////////////////

#include "base\base.h"
#include "mm\mm.h"
#include "media\media.h"
#include "engine\engine.h"

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////

template<class Type> Type* GHCRef(TRef<Type> p)
{
    if (p) {
        p->AddRef();
    }
    return p;
}

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////

extern "C" {
    //////////////////////////////////////////////////////////////////////////////
    //
    //  System
    //
    //////////////////////////////////////////////////////////////////////////////

    Export void mrtRelease(Counted* p)
    {
        p->Release();
    }

    Export double mrtGetTime()
    {
        return Base::GetSystemTime();
    }

    //////////////////////////////////////////////////////////////////////////////
    //
    //  Sockets
    //
    //////////////////////////////////////////////////////////////////////////////

    #ifdef Sockets
        //////////////////////////////////////////////////////////////////////////////
        //
        //  SocketAddress
        //
        //////////////////////////////////////////////////////////////////////////////

        Export Base::IPAddress mrtGetAddress(Base::SocketAddress* paddress)
        {
            return paddress->GetAddress();
        }

        Export Base::IPPort mrtGetPort(Base::SocketAddress* paddress)
        {
            return paddress->GetPort();
        }

        Export int mrtCompareSocketAddress(Base::SocketAddress* pax, Base::SocketAddress* pay)
        {
            return Base::CompareSocketAddress(pax, pay);
        }

        Export Base::SocketAddress* mrtCreateSocketAddress(Base::IPAddress addr, Base::IPPort port)
        {
            return GHCRef(Base::CreateSocketAddress(addr, port));
        }

        Export Base::SocketAddress* mrtGetSocketAddress(char* pname, Base::IPPort port)
        {
            return GHCRef(Base::GetSocketAddress(pname, port));
        }

        //////////////////////////////////////////////////////////////////////////////
        //
        //  UDPSocket
        //
        //////////////////////////////////////////////////////////////////////////////

        Export void mrtUDPClose(Base::UDPSocket* pudpSocket)
        {
            pudpSocket->Close();
        }

        Export void mrtUDPSocketBind(Base::UDPSocket* pudpSocket, Base::IPPort port)
        {
            pudpSocket->Bind(port);
        }

        Export uint mrtUDPSocketRead(Base::UDPSocket* pudpSocket, char* pv, uint length)
        {
            return pudpSocket->Read(pv, length);
        };

        Export uint mrtUDPSocketReadFrom(Base::UDPSocket* pudpSocket, char* pv, uint length, Base::SocketAddress** ppudpAddress)
        {
            uint count = pudpSocket->ReadFrom(pv, length, ppudpAddress);
            (*ppudpAddress)->AddRef();
            return count;
        }

        Export uint mrtUDPSocketWrite(Base::UDPSocket* pudpSocket, Base::SocketAddress* udpAddress, char* pv, uint length)
        {
            return pudpSocket->Write(udpAddress, pv, length);
        }

        Export Base::UDPSocket* mrtUDPSocketCreate()
        {
            return GHCRef(Base::CreateUDPSocket());
        }

        //////////////////////////////////////////////////////////////////////////////
        //
        //  TCPSocket
        //
        //////////////////////////////////////////////////////////////////////////////

        Export void mrtClose(Base::TCPSocket* psocket)
        {
            psocket->Close();
        }

        Export void mrtSetNoDelay(Base::TCPSocket* psocket, bool b)
        {
            psocket->SetNoDelay(b);
        }

        Export uint mrtRead(Base::TCPSocket* psocket, char* pv, uint length)
        {
            return psocket->Read(pv, length);
        }

        Export void mrtWrite(Base::TCPSocket* psocket, const char* pv, uint length)
        {
            return psocket->Write(pv, length);
        }

        Export Base::TCPSocket* mrtConnect(char* pname, Base::IPPort port)
        {
            return GHCRef(Base::Connect(pname, port));
        }

        Export Base::TCPSocket* mrtConnectSocketAddress(Base::IPAddress addr, Base::IPPort port)
        {
            return GHCRef(Base::Connect(addr, port));
        }

        //////////////////////////////////////////////////////////////////////////////
        //
        //  TCPListener
        //
        //////////////////////////////////////////////////////////////////////////////

        Export Base::TCPSocket* mrtListen(Base::TCPListener* ptcpPort)
        {
            return GHCRef(ptcpPort->Listen());
        }

        Export Base::TCPListener* mrtCreateTCPListener(Base::IPPort port)
        {
            return GHCRef(Base::CreateTCPListener(port));
        }
    #endif

    //////////////////////////////////////////////////////////////////////////////
    //
    //  Media
    //
    //////////////////////////////////////////////////////////////////////////////

    Export Media::Texture* mrtLoadTexture(PCWC pcs)
    {
        return GHCRef(Media::LoadTexture(pcs));
    }

    Export Media::PixelShader* mrtCreatePixelShader(PCC pcs, PCC pcsMain)
    {
        return GHCRef(Media::CreatePixelShader(pcs, pcsMain));
    }

    //////////////////////////////////////////////////////////////////////////////
    //
    //  Texture
    //
    //////////////////////////////////////////////////////////////////////////////

    Export void mrtTextureGetSize(Media::Texture* ptexture, PointUInt& size)
    {
        size = ptexture->GetSize();
    }

    //////////////////////////////////////////////////////////////////////////////
    //
    //  Window
    //
    //////////////////////////////////////////////////////////////////////////////

    Export Media::Window* mrtOpenWindow(const PointInt& offset, const PointUInt& size, PCWC pccTitle)
    {
        return GHCRef(Media::OpenWindow(offset, size, pccTitle));
    }

    Export void mrtCloseWindow(Media::Window* pwindow)
    {
        pwindow->Close();
    }

    Export int mrtWindowClosed(Media::Window* pwindow)
    {
        return pwindow->Closed();
    }

    //////////////////////////////////////////////////////////////////////////////
    //
    //  Input
    //
    //////////////////////////////////////////////////////////////////////////////

    Export void mrtHandleInput(Media::Window* pwindow, void (*pfnCallback)(int, int, int, int, int))
    {
        pwindow->HandleInput(pfnCallback);
    }

    //////////////////////////////////////////////////////////////////////////////
    //
    //  DeviceContext
    //
    //////////////////////////////////////////////////////////////////////////////

    Export Media::DeviceContext* mrtGetDeviceContext(Media::Window* pwindow)
    {
        return GHCRef(pwindow->GetDeviceContext());
    }

    //////////////////////////////////////////////////////////////////////////////
    //
    //  Engine
    //
    //////////////////////////////////////////////////////////////////////////////

    Export ENS::Engine* mrtCreateEngine(Media::DeviceContext* pdc) { return GHCRef(ENS::CreateEngine(pdc)); }

    Export int  mrtGetTrianglesPerDraw(ENS::Engine* pengine)                            { return int(pengine->GetTrianglesPerDraw()); }
    Export bool mrtBeginFrame         (ENS::Engine* pengine)                            { return pengine->BeginFrame(); }
    Export void mrtEndFrame           (ENS::Engine* pengine)                            { pengine->EndFrame(); }
    Export void mrtPushState          (ENS::Engine* pengine)                            { pengine->PushState(); }
    Export void mrtPopState           (ENS::Engine* pengine)                            { pengine->PopState(); }
    Export void mrtSetPipelineState   (ENS::Engine* pengine, Media::PipelineState* pps) { pengine->SetPipelineState(pps); }
    Export void mrtSetColor           (ENS::Engine* pengine, const Color& color)        { pengine->SetColor(color); }
    Export void mrtDrawTexture        (ENS::Engine* pengine, Media::Texture* ptexture)  { pengine->DrawTexture(ptexture); }
    Export void mrtFillRectangle      (ENS::Engine* pengine, const Rect& rect)          { pengine->FillRectangle(rect); }
    Export void mrtRotate             (ENS::Engine* pengine, float angle)               { pengine->Rotate(angle); }
    Export void mrtTranslate          (ENS::Engine* pengine, const V2& v)               { pengine->Translate(v); }
    Export void mrtScale              (ENS::Engine* pengine, const V2& v)               { pengine->Scale(v); }
    Export void mrtTransform          (ENS::Engine* pengine, const V2& v)               { pengine->TransformAxis(v); }
    Export void mrtClip               (ENS::Engine* pengine, const Rect& rect)          { pengine->Clip(&rect); }
    Export void mrtDrawTest           (ENS::Engine* pengine, float time, const V2& v)   { pengine->DrawTest(time, v); }

    //////////////////////////////////////////////////////////////////////////////
    //
    //  Font
    //
    //////////////////////////////////////////////////////////////////////////////

    Export Media::Font* mrtCreateFont(PCWC pname, float height, float weight, bool bItalic, bool bSubpixelplacement)
    {
        return GHCRef(Media::CreateFont(pname, height, weight, bItalic, bSubpixelplacement));
    }

    //////////////////////////////////////////////////////////////////////////////
    //
    //  Text
    //
    //////////////////////////////////////////////////////////////////////////////

    Export void mrtDrawText(ENS::Engine* pengine, Media::Font* pfont, PCWC pcs)
    {
        TRef<Media::Text> ptext = Media::CreateText(pfont, pcs, uint(wcslen(pcs)));
        pengine->DrawText(ptext);
    }

    Export void mrtTextBound(Media::Font* pfont, PCWC pcs, Rect& rect)
    {
        TRef<Media::Text> ptext = Media::CreateText(pfont, pcs, uint(wcslen(pcs)));
        rect = ptext->GetBound();
    }

    Export bool mrtHitTest(Media::Font* pfont, PCWC pcs, const V2& v, uint& index, Rect& rect)
    {
        TRef<Media::Text> ptext = Media::CreateText(pfont, pcs, uint(wcslen(pcs)));
        return ptext->HitTest(v, index, rect);
    }

    Export void mrtGetGlyphRect(Media::Font* pfont, PCWC pcs, uint index, Rect& rect)
    {
        TRef<Media::Text> ptext = Media::CreateText(pfont, pcs, uint(wcslen(pcs)));
        rect = ptext->GetGlyphRect(index);
    }

    //////////////////////////////////////////////////////////////////////////////
    //
    //
    //
    //////////////////////////////////////////////////////////////////////////////

    Export void mrtInitialize()
    {
        Base::Initialize();
        Media::Initialize();
    }

    Export void mrtUninitialize()
    {
        Media::Uninitialize();
        Base::Uninitialize();
    }
}
