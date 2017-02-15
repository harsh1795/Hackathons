<?php
try {
	include 'config.php';
	//echo "TYPE :::::: "+$type+"  :::::   ";
	$q_name = $_POST['q_name'];
	$name = $_POST['name'];
	$val = $_POST['val'];
	$conn= new PDO("mysql:host=$servername;dbname=$database;charset:utf8",$username,$password);
	$conn-> setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
	$conn-> setAttribute(PDO::ATTR_EMULATE_PREPARES, false);

	$stmt = $conn->prepare("SELECT * FROM answers WHERE name='$name'");
	$stmt->execute();
	if ($stmt->rowCount() >0) {
		$stmt1 = $conn->prepare("UPDATE `answers` SET `".$q_name."`='$val' WHERE `name`='$name'");
		$stmt1->execute();
		echo "0";
	}
	else{
		$stmt1 = $conn->prepare("INSERT INTO `answers`( `name`,`".$q_name."`) VALUES ('$name','$val')");
		$stmt1->execute();
		echo "1";
	}
} catch (PDOException $e) {
	echo "No Suggestions".$e;
}

?>