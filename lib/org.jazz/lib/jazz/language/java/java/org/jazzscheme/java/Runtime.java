package org.jazzscheme.java;

public class Runtime
{
    private static Runtime currentRuntime = new Runtime();
    
    public static Runtime getRuntime() { 
		return currentRuntime;
    }


	// Interface to Java calling back into Jazz
	public native int callJazz(int x);
	
	
	// Patch to an incredible JVM bug where calling Class.forName directly
	// from JNI will fail because forName relies on finding the class loader
	// of the third method in the call stack. By the stack has only two
	// methods on it: forName and forName0!!!
	public Class forName(String className) throws ClassNotFoundException {
		return Class.forName(className);
	}
	
	
	// Patch time again!
	public java.sql.Connection getConnection(String url, String user, String password) throws java.sql.SQLException {
		return java.sql.DriverManager.getConnection(url, user, password);
	}
}
