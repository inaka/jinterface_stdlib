package com.ericsson.otp.stdlib;

/**
 * The equivalent to returning {stop, Reason, state()} on a gen_server's
 * handle_call/cast/info
 * 
 * @author Fernando Benavides <elbrujohalcon@inaka.net>
 */
public class OtpStopException extends Exception {

	/**
	 * 
	 */
	private static final long	serialVersionUID	= 735167560386747312L;
}
