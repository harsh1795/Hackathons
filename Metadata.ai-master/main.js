$(document).ready(function(){
	$("#submit").css("display","none");
	$("#result_predicted").css("display","none");
	$("#Level_indicator").css("display","none");
	$("#form_final").css("display","none");
	$("#org_Level_indicator").css("display","none");
	
	level = $("#Level").val();
	console.log("Level"+level);
});
$('#submit_checkup').click(function(){
	$("#checkup_div").css("display","none");
	$("#submit").css("display","block");
	lvl = $("#Level").val();
	node = $("#Node").val();
	getquestion(lvl+" "+node);
	console.log("Level"+lvl);
})

function getquestion(qno){
	$.ajax({
    	url: "getquestion.php",
    	method:'GET',
    	data:{qno:qno},
    	success: function(result){
    		console.log(result);
    		result = JSON.parse(result);
    		$('#count').val(result.length);
    		$("#myForm").empty();
    		for (var i = result.length - 1; i >= 0; i--) {
    			qname = result[i].qname;
	        	type = result[i].type;
	        	ds = result[i].ds;
	        	console.log(":qname:"+qname+":type:"+type+":ds:"+ds);
	        	append = '<div id="q_block'+i+'"><h3>Question :</h3><span><h3 id="q_no'+i+'" type="'+type+'"  ds="'+ds+'" q_name="'+qname+'">'+qname+'</h3></span>'
	        	if (result[i].type == 0) {
	        		append += '<div id="form0"><input type="radio" name="'+qname+'" main="Yes" checked> Yes<br><input type="radio"  main="No" id="q_no" name="'+qname+'"> No<br></div>';
	        	}else{
	        		append +='<div id="form1"><input type="text" id="answer'+qname+'" main="'+qname+'"><br></div><p id="type"></p><p id="question_no"></p></div>';
	        	}
	        	append += '</div>';
	       		$("#myForm").append(append);
    		}

    	},
    	error: function(error){
        	console.log(error);
    	}
	});
}
function make_entry(name,q_name,val){
	$.ajax({
    	url: "addanswer.php",
    	method:'POST',
    	data:{name:name,val:val,q_name:q_name},
    	success: function(result){
    		//result = JSON.parse(result);
        	console.log(result);
    	},
    	error: function(error){
        	console.log(error);
    	}
	});
}
function make_entry_all(answers){
		name = $("#name").val();
		$.ajax({
	    	url: "addallanswers.php",
	    	method:'POST',
	    	data:{name:name,answers:answers},
	    	success: function(result){
	    		//result = JSON.parse(result);
	        	console.log(result);
	    	},
	    	error: function(error){
	        	console.log(error);
	    	}
		})
}
$('#r_submit').click(function(){
	lp_val = $("#pr").html();
	la_val = $('#ar :selected').val();
	
	node = la_val.charAt(1);
	level = la_val.charAt(0);
	console.log("Level"+level);
	lp = "L"+document.getElementById('org_Level').value+document.getElementById('org_Node').value+"P";
	la = "L"+document.getElementById('org_Level').value+document.getElementById('org_Node').value+"A";
	name = $("#name").val();
	make_entry(name,la,la_val);
	make_entry(name,lp,lp_val);
	org_Level = $("#org_Level").val(la_val.charAt(0));
    org_Node = $("#org_Node").val(la_val.charAt(1));
	checkups = $('#checkups').val();
	$('#checkups').val(checkups-1);
	if((checkups-1) == 0){	
		$("#Level").val(level);
		$("#Node").val(node);
		$("#myForm").empty();
		//getquestion(""+level+" "+node+"");
		$("#myForm").css("display","block");
		$('#checkup_div').css("display","block");
	}
	else{
		count = $('#count').val();
		console.log(count);
		answers = [];
		for (var i = count - 1; i >= 0; i--) {
			q_name = $("#q_no"+i).attr('q_name');
			ds = $("#q_no"+i).attr('ds');
			type = $("#q_no"+i).attr('type');
			console.log("qname"+q_name+"ds"+ds+"type"+type);
			if (ds == 0) {
				$("#q_block"+i).css("display","none");
			}
		}
		$("#myForm").css("display","block");
		$('#submit').css("display","block");
	}

	$("#result_predicted").css("display","none");
	$('#ar').empty();
})
$('#submit').click(function(){
	count = $('#count').val();
	console.log(count);
	answers = [];
	for (var i = count - 1; i >= 0; i--) {
		type = $("#q_no"+i).attr('type');
		q_name = $("#q_no"+i).attr('q_name');
		name = $("#name").val();
		if (type == 0) {
			//console.log("Loop 0");
			val = $('input[name="'+q_name+'"]:checked', '#myForm').attr('main');
			//console.log(val);
			//make_entry(name,q_name,val);
		}else{
			//console.log("Loop 1");
			val = document.getElementById("answer"+q_name).value;
			//make_entry(name,q_name,val); 
		}
		console.log("Type"+type+"Val"+val);
		answer = {"q_name":q_name,"val":val};
		answers.push(answer);
	}
	make_entry_all(answers);
	lp_val = $("#pr").html();
	lvl = $("#Level").val();
	node = $("#Node").val();
	console.log("Level"+lvl);
	if (lvl == 3) {
		python_train_final('/L'+lvl+node);
		$("#form_final").css("display","block");
		$("#myForm").css("display","none");
	}else{
		python_train('/L'+lvl+node);
		$("#myForm").css("display","none");
		$("#result_predicted").css("display","block");
		$('#submit').css("display","none");
		result = $("#pr").html();
	}
});
$("#submit_final").click(function(){
	lp_val = $('#pr_final').val();
	la_val = $('#final_select :selected').val();
	lp = "L"+document.getElementById('org_Level').value+document.getElementById('org_Node').value+"P";
	la = "L"+document.getElementById('org_Level').value+document.getElementById('org_Node').value+"A";
	name = $("#name").val();
	make_entry(name,la,la_val);
	make_entry(name,lp,lp_val);
	$.ajax({
    	url: "final_submit.php",
    	method:'POST',
    	data:{name:name},
    	success: function(result){
    		//result = JSON.parse(result);
        	console.log(result);
    	},
    	error: function(error){
        	console.log(error);
    	}
	});
	location.reload();
})
$("#train").click(function(){
	python_train('/');

});
function get_nodes(level){
	console.log("Level"+level);
	$.ajax({
    	url: "get_nodes.php",
    	method:'GET',
    	data:{level:level},
    	success: function(result){
    		console.log(result);
    		result = JSON.parse(result);
    		append="";
    		for (var i = result.length - 1; i >= 0; i--) {
    			append += '<option value="'+ result[i].Level+result[i].Node+'">'+result[i].Level+result[i].Node+'</option>';
    		}
    		$('#ar').append(append);
    	},
    	error: function(error){
        	console.log(error);
    	}
	});
}
function python_train(url){
	$.ajax({
    	url: "http://localhost:5000"+url,
    	method:'GET',
    	success: function(result){
        	console.log(result);
        	result= result.toString().split("|");
        	$("#pr").html(result[0]);
        	$("#pr_prob").html(result[1]);
			level = result[0].charAt(0);
			console.log("Level"+level);
			get_nodes(level);
    	},
    	error: function(error){
        	console.log(error);
    	}
	});
};
function python_train_final(url){
	$.ajax({
    	url: "http://localhost:5000"+url,
    	method:'GET',
    	success: function(result){
        	console.log(result);
        	$("#pr_final").html(result);
    	},
    	error: function(error){
        	console.log(error);
    	}
	});
};