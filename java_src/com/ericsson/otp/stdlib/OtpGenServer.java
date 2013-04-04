package com.ericsson.otp.stdlib;

import java.util.logging.Logger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

/**
 * An abstract class for implementing a gen_server-like process. It should work
 * as OTP gen_server. The methods you would usually implement on the callback
 * module are abstract here so you have to implement it when inheriting from
 * this class. Then, if you get hold of this process PID or registered name from
 * a linked Erlang node, you can use gen_server functions (e.g.
 * gen_server:call/2) with it.
 * 
 * @author Fernando Benavides <elbrujohalcon@inaka.net>
 */
public abstract class OtpGenServer extends OtpSysProcess {
	private static final Logger	jlog			= Logger.getLogger(OtpGenServer.class
														.getName());
	private static final long	DEFAULT_TIMEOUT	= 5000;
	/**
	 * Use it as an equivalent to Erlang's infinity atom
	 */
	public static final long	INFINITY		= 0;

	/**
	 * Equivalent to
	 * {@link OtpGenServer#call(OtpNode, String, String, OtpErlangObject, long)}
	 * with timeout = 5000
	 * 
	 * @param host
	 *            This node
	 * @param server
	 *            gen_server PID
	 * @param call
	 *            message to send
	 * @return the response from the server or null if timeout was met
	 * @throws OtpErlangException
	 *             If the response from the server is invalid
	 */
	public static OtpErlangObject call(OtpNode host, OtpErlangPid server,
			OtpErlangObject call) throws OtpErlangException {
		return call(host, server, call, DEFAULT_TIMEOUT);
	}

	/**
	 * Makes a synchronous call to the gen_server server by sending a request
	 * and waiting until a reply arrives or a timeout occurs. The gen_server
	 * will call Module:handle_call/3 to handle the request.
	 * 
	 * @param host
	 *            This node
	 * @param server
	 *            gen_server PID
	 * @param call
	 *            message to send
	 * @param timeout
	 *            Timeout is a long which specifies how many milliseconds to
	 *            wait for a reply, or 0 to wait indefinitely. If no reply is
	 *            received within the specified time, the function call returns
	 *            null.
	 * @return the response from the server or null if timeout was met
	 * @throws OtpErlangException
	 *             If the response from the server is invalid
	 */
	public static OtpErlangObject call(OtpNode host, OtpErlangPid server,
			OtpErlangObject call, long timeout) throws OtpErlangException {
		OtpMbox caller = host.createMbox();
		OtpErlangTuple from = new OtpErlangTuple(new OtpErlangObject[] {
				caller.self(), host.createRef() });
		OtpErlangObject msg = new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom("$gen_call"), from, call });
		caller.send(server, msg);
		OtpErlangObject res = timeout == 0 ? caller.receive() : caller
				.receive(timeout);
		if (res == null) {
			return null;
		} else if (res instanceof OtpErlangTuple
				&& ((OtpErlangTuple) res).arity() == 2) {
			return ((OtpErlangTuple) res).elementAt(1);
		} else {
			throw new OtpErlangException("Invalid Response: " + res);
		}
	}

	/**
	 * Equivalent to
	 * {@link OtpGenServer#call(OtpNode, String, String, OtpErlangObject, long)}
	 * with timeout = 5000
	 * 
	 * @param host
	 *            This node
	 * @param server
	 *            gen_server registered name
	 * @param node
	 *            node where the gen_server lives
	 * @param call
	 *            message to send
	 * @return the response from the server or null if timeout was met
	 * @throws OtpErlangException
	 *             If the response from the server is invalid
	 */
	public static OtpErlangObject call(OtpNode host, String server,
			String node, OtpErlangObject call) throws OtpErlangException {
		return call(host, server, node, call, DEFAULT_TIMEOUT);
	}

	/**
	 * Makes a synchronous call to the gen_server server by sending a request
	 * and waiting until a reply arrives or a timeout occurs. The gen_server
	 * will call Module:handle_call/3 to handle the request.
	 * 
	 * @param host
	 *            This node
	 * @param server
	 *            gen_server registered name
	 * @param node
	 *            node where the gen_server lives
	 * @param call
	 *            message to send
	 * @param timeout
	 *            Timeout is an long which specifies how many milliseconds to
	 *            wait for a reply, or 0 to wait indefinitely. If no reply is
	 *            received within the specified time, the function call returns
	 *            null.
	 * @return the response from the server or null if timeout was met
	 * @throws OtpErlangException
	 *             If the response from the server is invalid
	 */
	public static OtpErlangObject call(OtpNode host, String server,
			String node, OtpErlangObject call, long timeout)
			throws OtpErlangException {
		OtpMbox caller = host.createMbox();
		OtpErlangTuple from = new OtpErlangTuple(new OtpErlangObject[] {
				caller.self(), host.createRef() });
		OtpErlangObject msg = new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom("$gen_call"), from, call });
		caller.send(server, node, msg);
		OtpErlangObject res = timeout == 0 ? caller.receive() : caller
				.receive(timeout);
		if (res == null) {
			return null;
		} else if (res instanceof OtpErlangTuple
				&& ((OtpErlangTuple) res).arity() == 2) {
			return ((OtpErlangTuple) res).elementAt(1);
		} else {
			throw new OtpErlangException("Invalid Response: " + res);
		}
	}

	/**
	 * Sends an asynchronous request to the a gen_server and returns
	 * immediately, ignoring if the destination node or gen_server does not
	 * exist. The gen_server will call Module:handle_cast/2 to handle the
	 * request.
	 * 
	 * @param host
	 *            This node
	 * @param server
	 *            gen_server PID
	 * @param cast
	 *            message to send
	 */
	public static void cast(OtpNode host, OtpErlangPid server,
			OtpErlangObject cast) {
		OtpMbox caller = host.createMbox();
		OtpErlangObject msg = new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom("$gen_cast"), cast });
		caller.send(server, msg);
	}

	/**
	 * Sends an asynchronous request to the a gen_server and returns
	 * immediately, ignoring if the destination node or gen_server does not
	 * exist. The gen_server will call Module:handle_cast/2 to handle the
	 * request.
	 * 
	 * @param host
	 *            This node
	 * @param server
	 *            gen_server name
	 * @param node
	 *            node where the gen_server lives
	 * @param cast
	 *            message to send
	 */
	public static void cast(OtpNode host, String server, String node,
			OtpErlangObject cast) {
		OtpMbox caller = host.createMbox();
		OtpErlangObject msg = new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom("$gen_cast"), cast });
		caller.send(server, node, msg);
	}

	/**
	 * Returns a reply to the calling process
	 * 
	 * @param host
	 *            This Node
	 * @param from
	 *            The caller
	 * @param reply
	 *            The response to be sent
	 */
	public static void reply(OtpNode host, OtpErlangTuple from,
			OtpErlangObject reply) {
		OtpErlangPid to = (OtpErlangPid) from.elementAt(0);
		OtpErlangRef tag = (OtpErlangRef) from.elementAt(1);
		OtpErlangTuple tuple = new OtpErlangTuple((new OtpErlangObject[] { tag,
				reply }));
		host.createMbox().send(to, tuple);
	}

	/**********************************************************************************************/

	protected OtpGenServer(OtpNode host) {
		super(host);
	}

	protected OtpGenServer(OtpNode host, String name) {
		super(host, name);
	}

	@Override
	protected void loop() {
		boolean running = true;
		try {
			while (running) {
				try {
					OtpErlangObject o = this.getMbox().receive();
					jlog.finer("Received Request: " + o);
					running = decodeMsg(o);
				} catch (OtpErlangExit oee) {
					OtpErlangObject reason = oee.reason();
					jlog.warning("Linked process exited. Reason: " + reason);
					try {
						handleExit(oee);
					} catch (OtpStopException ose) {
						running = false;
					}
				}
			}
		} catch (OtpErlangException oee) {
			terminate(oee);
		}
		terminate(null);
	};

	private boolean decodeMsg(OtpErlangObject message)
			throws OtpErlangException {
		try {
			if (message instanceof OtpErlangTuple) {
				OtpErlangTuple msg = (OtpErlangTuple) message;
				OtpErlangAtom kind = (OtpErlangAtom) msg.elementAt(0);
				if (kind.atomValue().equals("system") && msg.arity() == 3) {
					OtpErlangTuple from = (OtpErlangTuple) msg.elementAt(1);
					OtpErlangObject req = msg.elementAt(2);
					handleSystemMessage(req, from);
				} else if (kind.atomValue().equals("$gen_call")
						&& ((OtpErlangTuple) message).arity() == 3) {
					OtpErlangTuple from = (OtpErlangTuple) msg.elementAt(1);
					OtpErlangObject cmd = msg.elementAt(2);
					try {
						OtpErlangObject reply = handleCall(cmd, from);
						reply(from, reply);
					} catch (OtpStopReplyException ose) {
						reply(from, ose.getResponse());
						return false;
					}
				} else if (kind.atomValue().equals("$gen_cast")
						&& ((OtpErlangTuple) message).arity() == 2) {
					OtpErlangObject cmd = msg.elementAt(1);
					handleCast(cmd);
				} else {
					handleInfo(message);
				}
			} else {
				handleInfo(message);
			}
		} catch (OtpContinueException ose) {
		} catch (OtpStopException ose) {
			return false;
		}
		return true;
	}

	@Override
	protected final OtpErlangList getMiscStatus() {
		OtpErlangTuple header = new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangAtom("header"),
				new OtpErlangString("Status for generic server "
						+ this.getMbox().getName()) });
		OtpErlangTuple data = new OtpErlangTuple(
				new OtpErlangObject[] {
						new OtpErlangAtom("data"),
						new OtpErlangList(
								new OtpErlangObject[] { getSpecificStatus() }) });
		OtpErlangList miscStatus = new OtpErlangList(new OtpErlangObject[] {
				header, data });
		return miscStatus;
	}

	/**
	 * Override to add specific information to get_status calls
	 * 
	 * @return a list of specific information to be included in the process
	 *         status description
	 */
	protected OtpErlangList getSpecificStatus() {
		return new OtpErlangList(new OtpErlangTuple(new OtpErlangObject[] {
				new OtpErlangString("State"),
				new OtpErlangString(this.toString()) }));
	}

	/**********************************************************************************************/

	protected abstract OtpErlangObject handleCall(OtpErlangObject cmd,
			OtpErlangTuple from) throws OtpStopException, OtpContinueException,
			OtpErlangException;

	protected abstract void handleCast(OtpErlangObject cmd)
			throws OtpStopException, OtpErlangException;

	protected abstract void handleInfo(OtpErlangObject cmd)
			throws OtpStopException;

	protected abstract void terminate(OtpErlangException oee);
}