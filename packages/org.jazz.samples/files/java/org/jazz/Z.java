package org.jazz;

import java.io.*;

public class Z extends Y implements J
{
	int x;
	long y;
	double z = 2.3;
	
	static {
		System.out.println("static init");
	}
	
	{
		if (x == 0)
			x = 5;
		else
		{
			System.out.println("instance init");
		}
	}
	
	Z()
	{
	}
	
	/* Hello */
	
	void i()
	{
		foo();
	}
	
	// Another comment
	
	void j()
	{
		i();
	}
	
	public static void main(String[] args)
	{
		System.out.println("Hello world");
	}
	
	class W extends Object
	{
		void j()
		{
		}
		
		void m()
		{
			i();
			println("asdf");
		}
		
		void n()
		{
		}
		
		void o()
		{
		}
	}
}

interface F
{
	void i()
	{
	}
	
	void q()
	{
	}
}
