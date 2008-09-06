package org.jazzscheme.test;

import java.sql.*;

public class Test
{
	int value;
	int value2;
	String name;
	
	public Test() {
		value = 100;
	}
	
	public Test(int v) {
		value = v;
		value2 = 5;
	}
	
	public Test(int v, int v2) {
		value = v;
		value2 = v2;
	}
	
	public Test(int v, String s) {
		value = v;
		value2 = 100;
	}
	
	public Test(String s, int v) {
		value = 500;
		value2 = v;
	}
	
	public int test() {
		return value + value2;
	}
	
	public int test(int x, int y) {
		return value + x * y;
	}
	
	public int test(int x, int y, int z) {
		return value + x * y * z;
	}
	
	public int cj(int x) {
		return org.jazzscheme.java.Runtime.getRuntime().callJazz(x);
	}
	
	public static void main(String[] args) {
		Test test = new Test(10);
		
		System.out.println(test.cj(2));
	}
	
	public int testJDBC(int x) throws ClassNotFoundException, SQLException {
		Class.forName("com.microsoft.jdbc.sqlserver.SQLServerDriver");
	//	org.jazzscheme.java.Runtime.getRuntime().forName("com.microsoft.jdbc.sqlserver.SQLServerDriver");
		
		DriverManager.getConnection("jdbc:microsoft:sqlserver://localhost:1433;databaseName=Northwind", "sa", "sa");
		
		return 23;
	}
}
