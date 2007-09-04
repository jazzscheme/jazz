tBool is_class_subtype(jType target, jType type)
{
	tInt	target_offset = target->class_ancestors_sizeGet();
	tInt	type_offset = type->class_ancestors_sizeGet();
	
	return	target_offset >= type_offset && 
			target->ancestorsGet()[type_offset-1] == type;
}



tBool is_interface_subtype(jType target, jType type)
{
	jTypePtr	ptr_start = target->ancestorsGet() + target->class_ancestors_sizeGet();
	jTypePtr	ptr = target->ancestorsGet() + target->ancestors_sizeGet();
	
	while (--ptr >= ptr_start)
		if (*ptr == type)
			return true;
	
	return false;
}
