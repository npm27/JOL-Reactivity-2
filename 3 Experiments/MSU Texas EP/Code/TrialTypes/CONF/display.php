<?php
    if (!isset($text) || $text === '') { 
        $text = 'How confident are you that the target word you just recalled is correct? | Type your response on a scale from 0-100 and then press ENTER. If you were unable to recall the previous target, please DO NOT type a number and instead only press ENTER.';
    }

    $texts = explode('|', $text);
    $mainText = array_shift($texts);
?>

<div class="textcenter">
    <div><?php echo isset($text) ? $text : ""; ?></div>

<br>

<div class="study">
    <span class="study-left"   ><?php echo $cue;    ?></span>
    <span class="study-divider"><?php echo "-";     ?></span>
    <span class="study-right"  ><?php echo "?"; ?></span>
    <br>

<br>
  
<div class="textcenter">
    <input name="JOL" type="text" value="" autocomplete="off" class="forceNumeric textcenter collectorInput">
    <button class="collectorButton" id="FormSubmitButton">Submit</button>
</div>
