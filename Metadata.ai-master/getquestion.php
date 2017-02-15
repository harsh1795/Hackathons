<?php
try {
	include 'config.php';
	//echo "TYPE :::::: "+$type+"  :::::   ";
	$qno = $_GET['qno'];
	$qno = explode(" ", $qno);
	$conn= new PDO("mysql:host=$servername;dbname=$database;charset:utf8",$username,$password);
	$conn-> setAttribute(PDO::ATTR_ERRMODE, PDO::ERRMODE_EXCEPTION);
	$conn-> setAttribute(PDO::ATTR_EMULATE_PREPARES, false);

	$stmt = $conn->prepare("SELECT * FROM questions WHERE Level='$qno[0]' AND Node='$qno[1]'");
	$stmt->execute();

	while($row= $stmt->fetch(PDO::FETCH_ASSOC)) {
		$Level = $row["Level"];
		$Node = $row["Node"];
		$qname = $row["Feature"];
		$type = $row['type'];
		$ds = $row['ds'];
		$arr[] = array("qname"=>$qname,"Level"=>$Level,"Node"=>$Node,"type"=>$type,"ds"=>$ds);
	}
	echo json_encode($arr);
} catch (PDOException $e) {
	echo "No Suggestions".$e;
}

?>