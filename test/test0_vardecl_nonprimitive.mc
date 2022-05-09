void main(){
    // empty array
    int[] a;
    var[][] b;
    printi(leng(a));
    printi(leng(b));
    // array size being implicitly defined by the length of literals
    var[] arr = [0, 2, 4, 6];
    string[] ar = ["H", "i"];
    printi(leng(arr));
    printi(leng(ar));

    var tupl = (1, 2.0, 3); //free declaration
    (int, float, bool) c = (4, 4.5, true);
    printi(leng(tupl));
    printi(leng(c));

    struct Student {
        name: string;
        age: int;
        major: string;
    }
    var student_info = {name: "Adam", age: 15, major: "Computer Science"};
    printf("Student’s name is {}, age is {}, and the major is {}",
        student_info.name, student_info.age, student_info.major);    



}