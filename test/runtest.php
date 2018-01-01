<?php

$tmpl = include 'test.php';

$data = array(
  'id' => 10,
  'url' => 'http://arru',
  'nm' => 'there is the name',
  'img' => 'http://imgur.com',
  'sz' => '100Кб',
  'ad' => 'после дождичка в четверг',
  'u' => 5,
  'd' => 6,
  'od' => 'длинное описание',
  'list' => array(
    array(
      'title' => 'first',
      'body' => 'fuck',
      'test' => true,
      'test2' => true
    ),
    array(
      'title' => 'second',
      'body' => 'азазаз',
      'test' => false,
      'test2' => true
    ),
    array(
      'title' => 'third',
      'body' => 'азазаз',
      'test' => false,
      'test2' => false
    )
  )
);

echo $tmpl($data);
