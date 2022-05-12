struct Student{
	int sid;
	string name;
	string major;
	float grade;
	bool graduated;

};

void main(){
	string str_a = 'Im student';
	prints(str_a);
	
	[] int a = [1,2,3];
	[] int aa = [];
	[] float b = [0.1, 0.2];
	[] string c = ['hello', 'hi', 'sup'];
	[] string cc = ['a', 'b', 'c'];

	Student x;
 	x.sid = 10025;
	x.name = 'Mark';
	x.major = 'Computer Science';
 	x.grade = 4.5;
 	x.graduated = false;

	prints(x.name);
	prints(x.major);
	prints(x.grade);
}
