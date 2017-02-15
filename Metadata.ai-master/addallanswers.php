<?php
try {
	include 'config.php';
	//echo "TYPE :::::: "+$type+"  :::::   ";
	$name = $_POST['name'];
	$answers = $_POST['answers'];
	//echo $name;
	//print_r($answers);
	$conn= new PDO("mysql:host=$servername;dbname=$database;charset:utf8",$username,$password);
	$conn-> setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
	$conn-> setAttribute(PDO::ATTR_EMULATE_PREPARES, false);
	$count = count($answers);

	for ($i=0; $i <$count ; $i++) { 
		$stmt = $conn->prepare("SELECT * FROM `answers` WHERE `name`='$name'");
		$stmt->execute();

		$val = $answers[$i]["val"];
		$q_name = $answers[$i]["q_name"];
		
		if ($stmt->rowCount() >0) {
				$stmt1 = $conn->prepare("UPDATE `answers` SET `".$q_name."`='$val' WHERE `name`='$name'");
				$stmt1->bindParam(":val",$val);
				$stmt1->bindParam(":name",$name);
				$stmt1->execute();
				echo "0";
		}
		else{
				$stmt1 = $conn->prepare("INSERT INTO `answers`( `name`,`".$q_name."`) VALUES ('$name','$val')");
				$stmt1->bindParam(":val",$val);
				$stmt1->bindParam(":name",$name);
				$stmt1->execute();
				echo "1";
		}
	}
} catch (PDOException $e) {
	echo "No Suggestions".$e;
}

?>