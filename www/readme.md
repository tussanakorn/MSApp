### MSAapp Installation

<head>
	<title>MSAapp Installation</title>
	<style>
		body {
			font-size: 18px;
		}
	</style>
</head>

<body>
	<p>The web-application can be used online or by installing it as an R package. For the online version, the web address is <a href="https://tussanakorn.shinyapps.io/msapp/">https://tussanakorn.shinyapps.io/msapp/</a>.</p>
	<p>To use the web application in R, the following steps should be carried out:</p>
	<ol>
		<li>Download and install R from the Comprehensive R Archive Network (CRAN) on <a href="https://www.rproject.org/">www.rproject.org</a>.</li>
		<li>For a better user experience, we recommend downloading and installing RStudio desktop, available at <a href="https://www.rstudio.com/">www.rstudio.com</a>.</li>
		<li>Finally, install MSAapp by running the following line of code in R: <code>devtools :: install_github("tussanakorn=MSApp"; ref = "main")</code>.</li>
	</ol>
	<p>Once MSAapp has been installed, this can be loaded in R by typing <code>library(MSAapp)</code> and <code>run_app()</code>, which will open a window as shown in Fig.</p>
</body>