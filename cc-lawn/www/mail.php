<?php
  /**
   * Send out a mail when requested
   *
   * Fire off a mail to my address when someone requests a contact
   */

foreach (array ('name', 'email', 'message', 'phone') as $req)
  {
    if (isset ($_POST[$req]) && empty ($_POST[$req]))
      {
        echo "All form fields are required, please <a href='#' onclick='javascript:history.go(-1);'>go back</a> and re-submit.";
        echo '<br>(missing '.$req.')<br>';
        die;
      }
  }

if (! preg_match ('/\d{10,}/', preg_replace ('/\D/', '', $_POST['phone'])))
  {
    echo 'Phone numbers must contain at least 10 digits, please fix.';
    die;
  }

if (! preg_match ('/\w.*@.*\.\w+/', $_POST['email']))
  {
    echo 'Please enter a valid email address.';
    die;
  }


//mail('3614434852@tmomail.net, slickfinch@yahoo.com, m@ahungry.com',
mail('m@ahungry.com',
     'Lawn In Order mail contact',
     "Lawn In Order mail contact\n".print_r($_POST, TRUE));

echo "Thank you for mailing, we will get back to you soon";