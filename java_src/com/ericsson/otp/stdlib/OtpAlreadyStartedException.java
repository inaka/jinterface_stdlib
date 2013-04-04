package com.ericsson.otp.stdlib;

import com.ericsson.otp.erlang.OtpException;

/**
 * A process with the desired name was already started
 * 
 * @author Fernando Benavides <elbrujohalcon@inaka.net>
 */
public class OtpAlreadyStartedException extends OtpException {

	/**
	 * Default constructor
	 * 
	 * @param name
	 *            Desired process name
	 */
	public OtpAlreadyStartedException(String name) {
		super("Process already started: " + name);
	}

	/**
	 * 
	 */
	private static final long	serialVersionUID	= 2061777348741972804L;

}
