float test(int b, float a){
	b = b + 1;
	printi(b);
	if(b > 5){
		return 0.0;
	}else{
		test(b, 1.9);
	}
	printf(a);
	return -1.9;
}

void main(){
	int i=0;
	
	test(0, 0.0);
}