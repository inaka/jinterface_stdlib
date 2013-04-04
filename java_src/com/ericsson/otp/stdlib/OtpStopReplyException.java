package com.ericsson.otp.stdlib;

import com.ericsson.otp.erlang.OtpErlangObject;

/**
 * The equivalent to returning {stop, Reason, Reply, state()} on a gen_server's
 * handle_call/cast/info
 * 
 * @author Fernando Benavides <elbrujohalcon@inaka.net>
 */
public class OtpStopReplyException extends OtpStopException {

	/**
	 * 
	 */
	private static final long		serialVersionUID	= 735167560386747312L;
	private final OtpErlangObject	response;

	/**
	 * @param response
	 *            the response to be sent to the caller before stopping
	 */
	public OtpStopReplyException(OtpErlangObject response) {
		super();
		this.response = response;
	}

	/**
	 * @return the response to be sent to the caller before stopping
	 */
	public OtpErlangObject getResponse() {
		return this.response;
	}

}
