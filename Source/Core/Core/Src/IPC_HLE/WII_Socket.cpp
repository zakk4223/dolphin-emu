// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#include "WII_Socket.h"
#include "WII_IPC_HLE.h"
#include "WII_IPC_HLE_Device.h"
// No Wii socket support while using NetPlay or TAS
#include "NetPlayProto.h"
#include "Movie.h"

using WII_IPC_HLE_Interface::ECommandType;
using WII_IPC_HLE_Interface::COMMAND_IOCTL;
using WII_IPC_HLE_Interface::COMMAND_IOCTLV;

#ifdef _WIN32
#define ERRORCODE(name) WSA ## name
#define EITHER(win32, posix) win32
#else
#define ERRORCODE(name) name
#define EITHER(win32, posix) posix
#endif

char* WiiSockMan::DecodeError(s32 ErrorCode)
{
#ifdef _WIN32
	static char Message[1024];
	// If this program was multi-threaded, we'd want to use FORMAT_MESSAGE_ALLOCATE_BUFFER
	// instead of a static buffer here.
	// (And of course, free the buffer when we were done with it)
	FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS |
		FORMAT_MESSAGE_MAX_WIDTH_MASK, NULL, ErrorCode,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPWSTR)Message, 1024, NULL);
	return Message;
#else
	return strerror(ErrorCode);
#endif
}

s32 translateErrorCode(s32 native_error, bool isRW)
{
	switch (native_error)
	{
	case ERRORCODE(EMSGSIZE):
		ERROR_LOG(WII_IPC_NET, "Find out why this happened, looks like PEEK failure?");
		return -1; // Should be -SO_EMSGSIZE
	case EITHER(WSAENOTSOCK, EBADF):
		return -SO_EBADF;
	case ERRORCODE(EADDRINUSE):
		return -SO_EADDRINUSE;
	case ERRORCODE(ECONNRESET):
		return -SO_ECONNRESET;
	case ERRORCODE(EISCONN):
		return -SO_EISCONN;
	case ERRORCODE(ENOTCONN):
		return -SO_EAGAIN; // After proper blocking SO_EAGAIN shouldn't be needed...
	case ERRORCODE(EINPROGRESS):
		return -SO_EINPROGRESS;
	case ERRORCODE(EALREADY):
		return -SO_EALREADY;
	case ERRORCODE(EACCES):
		return -SO_EACCES;
	case ERRORCODE(ECONNREFUSED):
		return -SO_ECONNREFUSED;
	case ERRORCODE(ENETUNREACH):
		return -SO_ENETUNREACH;
	case ERRORCODE(EHOSTUNREACH):
		return -SO_EHOSTUNREACH;
	case EITHER(WSAEWOULDBLOCK, EAGAIN):
		if (isRW){
			return -SO_EAGAIN;  // EAGAIN
		}else{
			return -SO_EINPROGRESS; // EINPROGRESS
		}
	default:
		return -1;
	}
}

s32 WiiSockMan::getNetErrorCode(s32 ret, std::string caller, bool isRW)
{
#ifdef _WIN32
	s32 errorCode = WSAGetLastError();
#else
	s32 errorCode = errno;
#endif

	if (ret >= 0)
	{
		WiiSockMan::getInstance().setLastNetError(ret);
		return ret;
	}

	INFO_LOG(WII_IPC_NET, "%s failed with error %d: %s, ret= %d",
		caller.c_str(), errorCode, DecodeError(errorCode), ret);

	s32 ReturnValue = translateErrorCode(errorCode, isRW);
	WiiSockMan::getInstance().setLastNetError(ReturnValue);

	return ReturnValue;
}

WiiSocket::~WiiSocket()
{
	if (fd >= 0)
	{
		(void)closeFd();
	}
}

void WiiSocket::setFd(s32 s)
{
	if (fd >= 0)
		(void)closeFd();

	nonBlock = false;
	fd = s;

	// Set socket to NON-BLOCK
#ifdef _WIN32
	u_long iMode = 1;
	ioctlsocket(fd, FIONBIO, &iMode);
#else
	int flags;
	if (-1 == (flags = fcntl(fd, F_GETFL, 0)))
		flags = 0;
	fcntl(fd, F_SETFL, flags | O_NONBLOCK);
#endif
}

s32 WiiSocket::closeFd()
{
	s32 ReturnValue = 0;
	if (fd >= 0)
	{
#ifdef _WIN32
		s32 ret = closesocket(fd);
#else
		s32 ret = close(fd);
#endif
		ReturnValue = WiiSockMan::getNetErrorCode(ret, "delSocket", false);
	}
	else
	{
		ReturnValue = WiiSockMan::getNetErrorCode(EITHER(WSAENOTSOCK, EBADF), "delSocket", false);
	}
	fd = -1;
	return ReturnValue;
}

s32 WiiSocket::_fcntl(u32 cmd, u32 arg)
{
#define F_GETFL			3
#define F_SETFL			4
#define F_NONBLOCK		4
	s32 ret = 0;
	if (cmd == F_GETFL)
	{
		ret = nonBlock ? F_NONBLOCK : 0;
	}
	else if (cmd == F_SETFL)
	{
		nonBlock = (arg & F_NONBLOCK) == F_NONBLOCK;
	}
	else
	{
		ERROR_LOG(WII_IPC_NET, "SO_FCNTL unknown command");
	}

	INFO_LOG(WII_IPC_NET, "IOCTL_SO_FCNTL(%08x, %08X, %08X)",
		fd, cmd, arg);

	return ret;
}

void WiiSocket::update(bool read, bool write, bool except)
{
	auto it = pending_sockops.begin();
	while (it != pending_sockops.end())
	{
		s32 ReturnValue = 0;
		bool forceNonBlock = false;
		ECommandType ct = static_cast<ECommandType>(Memory::Read_U32(it->_CommandAddress));
		if (!it->is_ssl && ct == COMMAND_IOCTL)
		{
			u32 BufferIn = Memory::Read_U32(it->_CommandAddress + 0x10);
			u32 BufferInSize = Memory::Read_U32(it->_CommandAddress + 0x14);
			u32 BufferOut = Memory::Read_U32(it->_CommandAddress + 0x18);
			u32 BufferOutSize = Memory::Read_U32(it->_CommandAddress + 0x1C);

			switch(it->net_type)
			{
			case IOCTL_SO_FCNTL:
			{
				u32 cmd	= Memory::Read_U32(BufferIn + 4);
				u32 arg	= Memory::Read_U32(BufferIn + 8);
				ReturnValue = _fcntl(cmd, arg);
				break;
			}
			case IOCTL_SO_BIND:
			{
				//u32 has_addr = Memory::Read_U32(BufferIn + 0x04);
				sockaddr_in local_name;
				WiiSockAddrIn* wii_name = (WiiSockAddrIn*)Memory::GetPointer(BufferIn + 0x08);
				WiiSockMan::Convert(*wii_name, local_name);

				int ret = bind(fd, (sockaddr*)&local_name, sizeof(local_name));
				ReturnValue = WiiSockMan::getNetErrorCode(ret, "SO_BIND", false);

				INFO_LOG(WII_IPC_NET, "IOCTL_SO_BIND (%08X %s:%d) = %d ", fd, 
					inet_ntoa(local_name.sin_addr), Common::swap16(local_name.sin_port), ret);
				break;
			}
			case IOCTL_SO_CONNECT:
			{
				//u32 has_addr = Memory::Read_U32(BufferIn + 0x04);
				sockaddr_in local_name;
				WiiSockAddrIn* wii_name = (WiiSockAddrIn*)Memory::GetPointer(BufferIn + 0x08);
				WiiSockMan::Convert(*wii_name, local_name);
				
				int ret = connect(fd, (sockaddr*)&local_name, sizeof(local_name));
				ReturnValue = WiiSockMan::getNetErrorCode(ret, "SO_CONNECT", false);

				INFO_LOG(WII_IPC_NET,"IOCTL_SO_CONNECT (%08x, %s:%d)",
					fd, inet_ntoa(local_name.sin_addr), Common::swap16(local_name.sin_port));
				break;
			}
			case IOCTL_SO_ACCEPT:
			{
		
				if (BufferOutSize > 0)
				{
					sockaddr_in local_name;
					WiiSockAddrIn* wii_name = (WiiSockAddrIn*)Memory::GetPointer(BufferOut);
					WiiSockMan::Convert(*wii_name, local_name);
					
					socklen_t addrlen = sizeof(sockaddr_in);
					int ret = (s32)accept(fd, (sockaddr*)&local_name, &addrlen);
					ReturnValue = WiiSockMan::getNetErrorCode(ret, "SO_ACCEPT", true);

					WiiSockMan::Convert(local_name, *wii_name, addrlen);
				}
				else
				{
					int ret = (s32)accept(fd, NULL, 0);
					ReturnValue = WiiSockMan::getNetErrorCode(ret, "SO_ACCEPT", true);
				}

				WiiSockMan::getInstance().addSocket(ReturnValue);

				INFO_LOG(WII_IPC_NET, "IOCTL_SO_ACCEPT "
					"BufferIn: (%08x, %i), BufferOut: (%08x, %i)",
					BufferIn, BufferInSize, BufferOut, BufferOutSize);

				break;
			}
			default:
				break;
			}

			// Fix blocking error codes
			if (!nonBlock)
			{
				if (it->net_type == IOCTL_SO_CONNECT 
					&& ReturnValue == -SO_EISCONN)
				{
					ReturnValue = SO_SUCCESS;
				}
			}
		}
		else if (ct == COMMAND_IOCTLV)
		{
			SIOCtlVBuffer CommandBuffer(it->_CommandAddress);
			u32 BufferIn = 0, BufferIn2 = 0;
			u32 BufferInSize = 0, BufferInSize2 = 0;
			u32 BufferOut = 0, BufferOut2 = 0;
			u32 BufferOutSize = 0, BufferOutSize2 = 0;
			
			if (CommandBuffer.InBuffer.size() > 0)
			{
				BufferIn = CommandBuffer.InBuffer.at(0).m_Address;
				BufferInSize = CommandBuffer.InBuffer.at(0).m_Size;
			}
			
			if (CommandBuffer.PayloadBuffer.size() > 0)
			{
				BufferOut = CommandBuffer.PayloadBuffer.at(0).m_Address;
				BufferOutSize = CommandBuffer.PayloadBuffer.at(0).m_Size;
			}
			
			if (CommandBuffer.PayloadBuffer.size() > 1)
			{
				BufferOut2 = CommandBuffer.PayloadBuffer.at(1).m_Address;
				BufferOutSize2 = CommandBuffer.PayloadBuffer.at(1).m_Size;
			}
			
			if (CommandBuffer.InBuffer.size() > 1)
			{
				BufferIn2 = CommandBuffer.InBuffer.at(1).m_Address;
				BufferInSize2 = CommandBuffer.InBuffer.at(1).m_Size;
			}

			if (it->is_ssl)
			{
				int sslID = Memory::Read_U32(BufferOut) - 1;
				if (SSLID_VALID(sslID))
				{
					switch(it->ssl_type)
					{
					case IOCTLV_NET_SSL_DOHANDSHAKE:
					{
					
						int ret = ssl_handshake(&CWII_IPC_HLE_Device_net_ssl::_SSL[sslID].ctx);
						switch (ret)
						{
						case 0:
							Memory::Write_U32(SSL_OK, BufferIn);
							break;
						case POLARSSL_ERR_NET_WANT_READ:
							Memory::Write_U32(SSL_ERR_RAGAIN, BufferIn);
							if (!nonBlock)
								ReturnValue = SSL_ERR_RAGAIN;
							break;
						case POLARSSL_ERR_NET_WANT_WRITE:
							Memory::Write_U32(SSL_ERR_WAGAIN, BufferIn);
							if (!nonBlock)
								ReturnValue = SSL_ERR_WAGAIN;
							break;
						default:
							Memory::Write_U32(SSL_ERR_FAILED, BufferIn);
							break;
						}
					
						INFO_LOG(WII_IPC_SSL, "IOCTLV_NET_SSL_DOHANDSHAKE = (%d) "
							"BufferIn: (%08x, %i), BufferIn2: (%08x, %i), "
							"BufferOut: (%08x, %i), BufferOut2: (%08x, %i)",
							ret,
							BufferIn, BufferInSize, BufferIn2, BufferInSize2,
							BufferOut, BufferOutSize, BufferOut2, BufferOutSize2);
						break;
					}
					case IOCTLV_NET_SSL_WRITE:
					{
						int ret = ssl_write(&CWII_IPC_HLE_Device_net_ssl::_SSL[sslID].ctx, Memory::GetPointer(BufferOut2), BufferOutSize2);
			
#ifdef DEBUG_SSL
						File::IOFile("ssl_write.bin", "ab").WriteBytes(Memory::GetPointer(BufferOut2), BufferOutSize2);
#endif
						if (ret >= 0)
						{
							// Return bytes written or SSL_ERR_ZERO if none
							Memory::Write_U32((ret == 0) ? SSL_ERR_ZERO : ret, BufferIn);
						}
						else 
						{
							switch (ret)
							{
							case POLARSSL_ERR_NET_WANT_READ:
								Memory::Write_U32(SSL_ERR_RAGAIN, BufferIn);
								if (!nonBlock)
									ReturnValue = SSL_ERR_RAGAIN;
								break;
							case POLARSSL_ERR_NET_WANT_WRITE:
								Memory::Write_U32(SSL_ERR_WAGAIN, BufferIn);
								if (!nonBlock)
									ReturnValue = SSL_ERR_WAGAIN;
								break;
							default:
								Memory::Write_U32(SSL_ERR_FAILED, BufferIn);
								break;
							}
						}
						break;
					}
					case IOCTLV_NET_SSL_READ:
					{
						int ret = ssl_read(&CWII_IPC_HLE_Device_net_ssl::_SSL[sslID].ctx, Memory::GetPointer(BufferIn2), BufferInSize2);
#ifdef DEBUG_SSL
						if (ret > 0)
						{
							File::IOFile("ssl_read.bin", "ab").WriteBytes(Memory::GetPointer(BufferIn2), ret);
						}
#endif
						if (ret >= 0)
						{
							// Return bytes read or SSL_ERR_ZERO if none
							Memory::Write_U32((ret == 0) ? SSL_ERR_ZERO : ret, BufferIn);
						}
						else 
						{
							switch (ret)
							{
							case POLARSSL_ERR_NET_WANT_READ:
								Memory::Write_U32(SSL_ERR_RAGAIN, BufferIn);
								if (!nonBlock)
									ReturnValue = SSL_ERR_RAGAIN;
								break;
							case POLARSSL_ERR_NET_WANT_WRITE:
								Memory::Write_U32(SSL_ERR_WAGAIN, BufferIn);
								if (!nonBlock)
									ReturnValue = SSL_ERR_WAGAIN;
								break;
							default:
								Memory::Write_U32(SSL_ERR_FAILED, BufferIn);
								break;
							}
						}
						break;
					}
					default:
						break;
					}
				}
				else
				{
					Memory::Write_U32(SSL_ERR_ID, BufferIn);
				}
			} 
			else
			{
				switch (it->net_type)
				{
				case IOCTLV_SO_SENDTO:
				{
				
					u32 flags = Memory::Read_U32(BufferIn2 + 0x04);
					u32 has_destaddr = Memory::Read_U32(BufferIn2 + 0x08);
					char * data = (char*)Memory::GetPointer(BufferIn);
					
					// Act as non blocking when SO_MSG_NONBLOCK is specified
					forceNonBlock = ((flags & SO_MSG_NONBLOCK) == SO_MSG_NONBLOCK);
					// send/sendto only handles MSG_OOB
					flags &= SO_MSG_OOB;
					
					sockaddr_in local_name = {0};
					if (has_destaddr)
					{						
						WiiSockAddrIn* wii_name = (WiiSockAddrIn*)Memory::GetPointer(BufferIn2 + 0x0C);
						WiiSockMan::Convert(*wii_name, local_name);
					}

					int ret = sendto(fd, data, BufferInSize, flags, 
						has_destaddr ? (struct sockaddr*)&local_name : NULL, 
						has_destaddr ? sizeof(sockaddr) :  0);
					ReturnValue = WiiSockMan::getNetErrorCode(ret, "SO_SENDTO", true);

					INFO_LOG(WII_IPC_NET,
						"%s = %d Socket: %08x, BufferIn: (%08x, %i), BufferIn2: (%08x, %i), %u.%u.%u.%u",
						has_destaddr ? "IOCTLV_SO_SENDTO " : "IOCTLV_SO_SEND ",
						ReturnValue, fd, BufferIn, BufferInSize,
						BufferIn2, BufferInSize2,
						local_name.sin_addr.s_addr & 0xFF,
						(local_name.sin_addr.s_addr >> 8) & 0xFF,
						(local_name.sin_addr.s_addr >> 16) & 0xFF,
						(local_name.sin_addr.s_addr >> 24) & 0xFF
						);
					break;
				}
				case IOCTLV_SO_RECVFROM:
				{
					u32 flags = Memory::Read_U32(BufferIn + 0x04);
					char * data = (char *)Memory::GetPointer(BufferOut);
					int data_len = BufferOutSize;

					sockaddr_in local_name;
					memset(&local_name, 0, sizeof(sockaddr_in));
			
					if (BufferOutSize2 != 0)
					{
						WiiSockAddrIn* wii_name = (WiiSockAddrIn*)Memory::GetPointer(BufferOut2);
						WiiSockMan::Convert(*wii_name, local_name);
					}

					// Act as non blocking when SO_MSG_NONBLOCK is specified
					forceNonBlock = ((flags & SO_MSG_NONBLOCK) == SO_MSG_NONBLOCK);
			
					// recv/recvfrom only handles PEEK/OOB
					flags &= SO_MSG_PEEK | SO_MSG_OOB;
#ifdef _WIN32
					if (flags & SO_MSG_PEEK){
						unsigned long totallen = 0;
						ioctlsocket(fd, FIONREAD, &totallen);
						ReturnValue = totallen;
						break;
					}
#endif
					socklen_t addrlen = sizeof(sockaddr_in);
					int ret = recvfrom(fd, data, data_len, flags,
									BufferOutSize2 ? (struct sockaddr*) &local_name : NULL,
									BufferOutSize2 ? &addrlen : 0);
					ReturnValue = WiiSockMan::getNetErrorCode(ret, BufferOutSize2 ? "SO_RECVFROM" : "SO_RECV", true);

			
					INFO_LOG(WII_IPC_NET, "%s(%d, %p) Socket: %08X, Flags: %08X, "
					"BufferIn: (%08x, %i), BufferIn2: (%08x, %i), "
					"BufferOut: (%08x, %i), BufferOut2: (%08x, %i)", 
					BufferOutSize2 ? "IOCTLV_SO_RECVFROM " : "IOCTLV_SO_RECV ",
					ReturnValue, data, fd, flags,
					BufferIn, BufferInSize, BufferIn2, BufferInSize2,
					BufferOut, BufferOutSize, BufferOut2, BufferOutSize2);

					if (BufferOutSize2 != 0)
					{
						WiiSockAddrIn* wii_name = (WiiSockAddrIn*)Memory::GetPointer(BufferOut2);
						WiiSockMan::Convert(local_name, *wii_name, addrlen);
					}
					break;
				}
				default:
					break;
				}
			}
			
		}

		if ( nonBlock || forceNonBlock
			|| (!it->is_ssl && ReturnValue != -SO_EAGAIN && ReturnValue != -SO_EINPROGRESS && ReturnValue != -SO_EALREADY)
			|| (it->is_ssl && ReturnValue != SSL_ERR_WAGAIN && ReturnValue != SSL_ERR_RAGAIN))
		{
			DEBUG_LOG(WII_IPC_NET, "IOCTL(V) Sock: %08x ioctl/v: %d returned: %d nonBlock: %d forceNonBlock: %d", 
				fd, it->is_ssl ? (int) it->ssl_type : (int) it->net_type, ReturnValue, nonBlock, forceNonBlock);
			WiiSockMan::EnqueueReply(it->_CommandAddress, ReturnValue);
			it = pending_sockops.erase(it);
		}
		else
		{
			++it;
		}
	}
}

void WiiSocket::doSock(u32 _CommandAddress, NET_IOCTL type)
{
	sockop so = {_CommandAddress, false};
	so.net_type = type;
	pending_sockops.push_back(so);
}

void WiiSocket::doSock(u32 _CommandAddress, SSL_IOCTL type)
{
	sockop so = {_CommandAddress, true};
	so.ssl_type = type;
	pending_sockops.push_back(so);
}

void WiiSockMan::addSocket(s32 fd)
{
	if (fd >= 0)
	{
		WiiSocket& sock = WiiSockets[fd];
		sock.setFd(fd);
	}
}

s32 WiiSockMan::newSocket(s32 af, s32 type, s32 protocol)
{
	if (NetPlay::IsNetPlayRunning()
		|| Movie::IsRecordingInput()
		|| Movie::IsPlayingInput())
	{
		return SO_ENOMEM;
	}

	s32 fd = (s32)socket(af, type, protocol);
	s32 ret = getNetErrorCode(fd, "newSocket", false);
	addSocket(ret);
	return ret;
}

s32 WiiSockMan::delSocket(s32 s)
{
	s32 ReturnValue = WiiSockets[s].closeFd();
	WiiSockets.erase(s);
	return ReturnValue;
}

void WiiSockMan::Update()
{
	s32 nfds = 0;
	fd_set read_fds, write_fds, except_fds;
	struct timeval t = {0,0};
	FD_ZERO(&read_fds);
	FD_ZERO(&write_fds);
	FD_ZERO(&except_fds);
	for (auto it = WiiSockets.begin(); it != WiiSockets.end(); ++it)
	{
		WiiSocket& sock = it->second;
		if (sock.valid())
		{
			FD_SET(sock.fd, &read_fds);
			FD_SET(sock.fd, &write_fds);
			FD_SET(sock.fd, &except_fds);
			nfds = max(nfds, sock.fd+1);
		}
		else
		{
			// Good time to clean up invalid sockets.
			WiiSockets.erase(sock.fd);
		}
	}
	s32 ret = select(nfds, &read_fds, &write_fds, &except_fds, &t);

	if (ret >= 0)
	{
		for (auto it = WiiSockets.begin(); it != WiiSockets.end(); ++it)
		{
			WiiSocket& sock = it->second;
			sock.update(
				FD_ISSET(sock.fd, &read_fds) != 0, 
				FD_ISSET(sock.fd, &write_fds) != 0, 
				FD_ISSET(sock.fd, &except_fds) != 0
			);
		}
	}
	else
	{
		for (auto it = WiiSockets.begin(); it != WiiSockets.end(); ++it)
		{
			it->second.update(false, false, false);
		}
	}
}

void WiiSockMan::EnqueueReply(u32 CommandAddress, s32 ReturnValue)
{
	Memory::Write_U32(8, CommandAddress);
	// IOS seems to write back the command that was responded to
	Memory::Write_U32(Memory::Read_U32(CommandAddress), CommandAddress + 8);
	
	// Return value
	Memory::Write_U32(ReturnValue, CommandAddress + 4);
	
	WII_IPC_HLE_Interface::EnqReply(CommandAddress);
}


void WiiSockMan::Convert(WiiSockAddrIn const & from, sockaddr_in& to)
{
	to.sin_addr.s_addr = from.addr.addr;
	to.sin_family = from.family;
	to.sin_port = from.port;
}

void WiiSockMan::Convert(sockaddr_in const & from, WiiSockAddrIn& to, s32 addrlen)
{
	to.addr.addr = from.sin_addr.s_addr;
	to.family = from.sin_family & 0xFF;
	to.port = from.sin_port;
	if (addrlen < 0 || addrlen > (s32) sizeof(WiiSockAddrIn))
		to.len = sizeof(WiiSockAddrIn);
	else
		to.len = addrlen;
}

#undef ERRORCODE
#undef EITHER
