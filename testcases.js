



//ODD EVEN

let num = 7;
if (num % 2 === 0) {
console.log(num + " is Even");
} 
else {
    console.log(num + " is Odd");
}
END






//TRAINGLE PATTERN

for (let i = 1; i <= 5; i++) { 
 let row = "";

 for (let j = 1; j <= i; j++) {
 row += "*";
 }

    console.log(row); 
}
END



//ARMSTRONG NO

function isArmstrong(num) { let temp = num; let sum = 0; 
while (temp > 0) {
    let digit = temp % 10;
    sum += digit *digit*digit;
    temp = Math.floor(temp / 10);
}

return sum == num;

}
console.log(isArmstrong(153));
console.log(isArmstrong(123));
END




//REVERSE

let arr = [1, 2, 3, 4, 5];
let reversed = [...arr].reverse();
console.log("Original: " + arr.join(", "));
console.log("Reversed: " + reversed.join(", "));
END





//PALINDROME

let str = "racecar";
let rev = str.split("").reverse().join("");
if(str == rev){
console.log(str ," is palindrome");
}
else{
console.log(str, " is NOT palindrome");
}
END