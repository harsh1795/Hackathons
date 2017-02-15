<?php
try {
	include 'config.php';
	//echo "TYPE :::::: "+$type+"  :::::   ";
	$name = $_POST['name'];
	$conn= new PDO("mysql:host=$servername;dbname=$database;charset:utf8",$username,$password);
	$conn-> setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
	$conn-> setAttribute(PDO::ATTR_EMULATE_PREPARES, false);


	$stmt1 = $conn->prepare("UPDATE `answers` SET `finished`='1' WHERE `name`='$name'");
	$stmt1->execute();

} catch (PDOException $e) {
	echo "No Suggestions".$e;
}

?>