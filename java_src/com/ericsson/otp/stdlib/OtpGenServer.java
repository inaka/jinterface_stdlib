package com.ericsson.otp.stdlib;

import java.util.logging.Logger;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

/**
 * @author Fernando Benavides <elbrujohalcon@inaka.net>
 * @doc An abstract class for implementing the server of a client-server
 *      relation. A generic server process (gen_server) implemented using this
 *      module will have a standard set of interface functions and include
 *      functionality for tracing and error reporting. It will also fit into an
 *      OTP supervision tree. Refer to OTP Design Principles for more
 *      information.
 */
public abstract class OtpGenServer {
	private static final Logger	jlog			= Logger.getLogger(OtpGenServer.class
														.getName());
	private static final long	DEFAULT_TIMEOUT	= 5000;
	/**
	 * Use it as an equivalent to Erlang's infinity atom
	 */
	public static final long	INFINITY		= 0;
	private OtpMbox				mbox;

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
	 *            Timeout is an long which specifies how many milliseconds to
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
	 * Default constructor
	 * 
	 * @param host
	 *            Node where the gen_server will leave
	 * @param name
	 *            Name to which the gen_server will be registered
	 */
	public OtpGenServer(OtpNode host, String name) {
		mbox = host.createMbox(name);
	}

	protected final OtpErlangPid getSelf() {
		return mbox.self();
	}

	/**
	 * Gets the server running
	 * 
	 * @throws OtpErlangException
	 *             if something fails :)
	 */
	public void start() throws OtpErlangException {
		boolean running = true;
		while (running) {
			try {
				jlog.fine("Receiving Requests at " + mbox.getName());
				OtpErlangObject o = mbox.receive();
				jlog.finer("Received Request: " + o);
				if (o instanceof OtpErlangTuple) {
					OtpErlangTuple msg = (OtpErlangTuple) o;
					OtpErlangAtom kind = (OtpErlangAtom) msg.elementAt(0);
					if (kind.atomValue().equals("$gen_call")
							&& ((OtpErlangTuple) o).arity() == 3) {
						OtpErlangTuple from = (OtpErlangTuple) msg.elementAt(1);
						OtpErlangObject cmd = msg.elementAt(2);
						OtpErlangObject reply = handleCall(cmd, from);
						reply(from, reply);
					} else if (kind.atomValue().equals("$gen_cast")
							&& ((OtpErlangTuple) o).arity() == 2) {
						OtpErlangObject cmd = msg.elementAt(1);
						handleCast(cmd);
					} else {
						handleInfo(o);
					}
				} else {
					handleInfo(o);
				}
			} catch (OtpErlangExit oee) {
				OtpErlangObject reason = oee.reason();
				jlog.warning("Linked process exited. Reason: " + reason);
				try {
					handleExit(oee);
				} catch (OtpStopException ose) {
					jlog.fine("Server stopping normally");
					running = false;
				}
			} catch (OtpContinueException ose) {
				running = true;
			} catch (OtpStopException ose) {
				jlog.fine("Server stopping normally");
				running = false;
			}
		}
		jlog.fine("...leaving");
	}

	protected void handleExit(OtpErlangExit oee) throws OtpErlangExit,
			OtpStopException {
		throw oee;
	}

	/**
	 * Returns a reply to the calling process
	 * 
	 * @param from
	 *            The calling process
	 * @param reply
	 *            Response to send
	 */
	protected void reply(OtpErlangTuple from, OtpErlangObject reply) {
		OtpErlangPid to = (OtpErlangPid) from.elementAt(0);
		OtpErlangRef tag = (OtpErlangRef) from.elementAt(1);
		OtpErlangTuple tuple = new OtpErlangTuple((new OtpErlangObject[] { tag,
				reply }));
		mbox.send(to, tuple);
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

	protected abstract OtpErlangObject handleCall(OtpErlangObject cmd,
			OtpErlangTuple from) throws OtpStopException, OtpContinueException,
			OtpErlangException;

	protected abstract void handleCast(OtpErlangObject cmd)
			throws OtpStopException, OtpErlangException;

	protected abstract void handleInfo(OtpErlangObject cmd)
			throws OtpStopException;
}
