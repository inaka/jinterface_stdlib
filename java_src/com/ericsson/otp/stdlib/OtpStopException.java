package com.ericsson.otp.stdlib;

/**
 * @author Fernando Benavides <elbrujohalcon@inaka.net>
 * @doc The equivalent to returning {stop, Reason, state()} on a gen_server's
 *      handle_call/cast/info
 */
public class OtpStopException extends Exception {

	/**
	 * 
	 */
	private static final long	serialVersionUID	= 735167560386747312L;
}
