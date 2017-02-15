<?php
try {
	include 'config.php';
	//echo "TYPE :::::: "+$type+"  :::::   ";
	$level = $_GET['level'];
	$conn= new PDO("mysql:host=$servername;dbname=$database;charset:utf8",$username,$password);
	$conn-> setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
	$conn-> setAttribute(PDO::ATTR_EMULATE_PREPARES, false);

	$stmt = $conn->prepare("SELECT DISTINCT Level,Node FROM questions WHERE Level='$level'");
	$stmt->execute();

	while($row= $stmt->fetch(PDO::FETCH_ASSOC)) {
		$Level = $row["Level"];
		$Node = $row["Node"];
		$arr[] = array("Level"=>$Level,"Node"=>$Node);
	}
	echo json_encode($arr);
} catch (PDOException $e) {
	echo "No Suggestions".$e;
}

?>