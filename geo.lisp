(in-package #:shopper)

(defparameter *geos*
  '(ireland europe us rest-of-world))

(ele:defpclass geography (cms)
  ((geography-name :initarg :name :accessor geo-name :index t)
   (geography-members :initarg :members :initform nil :accessor geo-members :index t)
   (providers :initform nil :accessor geo-providers)))

(defmethod title ((geo geography))
  (geo-name geo))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High level API
(defmethod get-object ((geo (eql :geography)) identifier)
  (get-geo identifier))

(defmethod get-all-objects ((geo (eql :geography)))
  (ele:get-instances-by-value 'geography 'store (store-name *web-store*)))

(defmethod get-identifier ((geo geography))
  (get-webform (geo-name geo)))

(defmethod get-form ((geo (eql :geography)))
  (geo-form))

(defmethod get-form ((geo geography))
  (geo-form geo))

(defmethod get-edit-url ((geo geography))
  (get-edit-page-url geo :countries))

(defmethod get-edit-tabs ((geo geography))
  '(:countries :postage-rates))


;; (defmethod edit-object ((geo geography) (page (eql :postage-rates)))
;;   (geo-postage-page geo (get-parameters*)))
(defmethod view-object ((obj geography))
  (with-html-output-to-string (s)
    (:h1 (fmt "Geography: ~A" (geo-name obj)))
    (:h3 "Countries")
    (fmt "~{~A~^, ~}."
	 (mapcar (compose #'cdr
			  #'get-country-info-from-iso-code)
		 (geo-members obj)))
    (:h3 "Providers")
    (:table :class "table"
	    (dolist (p (geo-providers obj))
	      (htm (:tr (:td (:strong (str (provider-name p)))) (:td "") (:td "")))
	      (dolist (rate (get-postage-rates p))
		(htm (:tr (:td "") (:td (fmt "~Ag" (second rate))) (:td (str (print-price (first rate)))))))))))

(defmethod edit-object ((obj geography))
  (when-let (params (fix-alist (post-parameters*)))
    (maybe-update obj params))
  (maybe-update-providers obj)
  (let* ((p (get-parameters*))
	 (countries-pane (if p "tab-pane" "tab-pane active"))
	 (providers-pane (if p "tab-pane active" "tab-pane"))
	 (countries-tab (if p "" "active"))
	 (providers-tab (if p "active" "")))
    (with-html-output-to-string (s)
      (:div :class "tabbable"
	    (:ul :class "nav nav-tabs"
		 (:li :class countries-tab
		      (:a :href "#countries" :data-toggle "tab" "Countries"))
		 (:li :class providers-tab
		      (:a :href "#providers" :data-toggle "tab" "Postage providers")))
	    (:div :class "tab-content"
		  (:div :class countries-pane :id "countries"
			(:a :name "countries" (:h2 "Countries"))
			(str (geo-form obj)))
		  (:div :class providers-pane :id "providers"
			(:a :name "providers" (:h2 "Providers"))
			(str (geo-postage-page obj))))))))



(defmethod edit-multiple-objects ((geo (eql :geography)) objs)
  (declare (ignore objs))
  (edit-geographies-page))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(ele:defpclass provider (quantity-list)
  ((provider-name :initarg :name :accessor provider-name :index t)))

(defun provider-entry-weight (provider-entry)
  (qlist-entry-quantity provider-entry))

(defun provider-entry-price (provider-entry)
  (qlist-entry-item provider-entry))

(defmethod add-postage (weight-limit price (provider provider))
  (add-item price provider weight-limit))

(defmethod get-postage-rates ((provider provider))
  (sort (copy-list (items provider)) #'< :key #'qlist-entry-quantity))

;; (defmethod get-url ((geo geography))
;;   (url-rewrite:add-get-param-to-url "/geo" "n" (get-webform (geo-name geo))))

(defun get-geo (webform)
  (find webform (get-all-objects :geography)
	:key (lambda (i) (get-webform (geo-name i)))
	:test #'string-equal))

(defun get-postage (weight provider)
  "Takes a weight (in grams) and a geo.  Returns postage for the item
in that geo"
  (labels ((qweight (e) (qlist-entry-quantity (car e)))
	   (get-rate (weight list-of-weights)
	     (if (null list-of-weights)
		 nil
		 (if (> weight (qweight list-of-weights))
		       (get-rate weight (cdr list-of-weights))
		       (car list-of-weights)))))
    (get-rate weight (get-postage-rates provider))))

(defun get-applicable-postage-rates (weight geography)
  (remove nil (mapcar (lambda (provider)
			   (cons (provider-name provider)
				 (car (get-postage weight provider))))
			 (geo-providers geography))
	  :key #'cdr))

(defun all-geographies ()
  (ele:get-instances-by-class 'geography))

(defun update-geos (item post-parameters)
  "update the geographies for an item from post-parameters.  Geography
  checkboxes start with G_"
  (setf (geographies item)
	(remove nil
		(mapcar (compose (lambda (geo-label)
				   (ele:get-instance-by-value 'geography
							      'geography-name geo-label))
				 #'url-decode
				 (lambda (name)
				   (subseq name 2))
				 #'car)
			(remove-if-not (lambda (parameter)
					 (and (stringp parameter)
					      (> (length parameter) 2)
					      (string-equal (subseq parameter 0 2) "G_")))
				       post-parameters
				       :key #'car)))))






(defun tidy-country (string)
  (string-trim '(#\Space #\Tab)
	       (string-capitalize string)))

(defun get-geo-from-country-code (code)
  (find-if (lambda (geo)
	     (country-in? code geo))
	   (get-all-objects :geography)))

(defun get-country-name-from-iso-code (iso-code)
  (cdr (get-country-info-from-iso-code iso-code)))

(defun get-country-info-from-iso-code (iso-code)
  "Returns alist (code . name)"
  (when-let (country-info (assoc iso-code *iso-3166-1-country-codes*
				 :test #'string-equal))
    (cons (car country-info) (tidy-country (cadr country-info)))))

(defun all-countries-info ()
  (mapcar (lambda (country-info)
	    (cons (car country-info) (tidy-country (cadr country-info))))
	  *iso-3166-1-country-codes*))

(defparameter *iso-3166-1-country-codes*
  '(("AX"  "AALAND ISLANDS                              " "ALA"     248)
    ("AF"  "AFGHANISTAN                                 " "AFG"     004)
    ("AL"  "ALBANIA                                     " "ALB"     008)
    ("DZ"  "ALGERIA                                     " "DZA"     012)
    ("AS"  "AMERICAN SAMOA                              " "ASM"     016)
    ("AD"  "ANDORRA                                     " "AND"     020)
    ("AO"  "ANGOLA                                      " "AGO"     024)
    ("AI"  "ANGUILLA                                    " "AIA"     660)
    ("AQ"  "ANTARCTICA                                  " "ATA"     010)
    ("AG"  "ANTIGUA AND BARBUDA                         " "ATG"     028)
    ("AR"  "ARGENTINA                                   " "ARG"     032)
    ("AM"  "ARMENIA                                     " "ARM"     051)  
    ("AW"  "ARUBA                                       " "ABW"     533)
    ("AU"  "AUSTRALIA                                   " "AUS"     036)
    ("AT"  "AUSTRIA                                     " "AUT"     040)
    ("AZ"  "AZERBAIJAN                                  " "AZE"     031)  
    ("BS"  "BAHAMAS                                     " "BHS"     044)
    ("BH"  "BAHRAIN                                     " "BHR"     048)
    ("BD"  "BANGLADESH                                  " "BGD"     050)
    ("BB"  "BARBADOS                                    " "BRB"     052)
    ("BY"  "BELARUS                                     " "BLR"     112)  
    ("BE"  "BELGIUM                                     " "BEL"     056)
    ("BZ"  "BELIZE                                      " "BLZ"     084)
    ("BJ"  "BENIN                                       " "BEN"     204)
    ("BM"  "BERMUDA                                     " "BMU"     060)
    ("BT"  "BHUTAN                                      " "BTN"     064)
    ("BO"  "BOLIVIA                                     " "BOL"     068)
    ("BA"  "BOSNIA AND HERZEGOWINA                      " "BIH"     070)
    ("BW"  "BOTSWANA                                    " "BWA"     072)
    ("BV"  "BOUVET ISLAND                               " "BVT"     074)
    ("BR"  "BRAZIL                                      " "BRA"     076)
    ("IO"  "BRITISH INDIAN OCEAN TERRITORY              " "IOT"     086)
    ("BN"  "BRUNEI DARUSSALAM                           " "BRN"     096)
    ("BG"  "BULGARIA                                    " "BGR"     100)
    ("BF"  "BURKINA FASO                                " "BFA"     854)
    ("BI"  "BURUNDI                                     " "BDI"     108)
    ("KH"  "CAMBODIA                                    " "KHM"     116)
    ("CM"  "CAMEROON                                    " "CMR"     120)
    ("CA"  "CANADA                                      " "CAN"     124)
    ("CV"  "CAPE VERDE                                  " "CPV"     132)
    ("KY"  "CAYMAN ISLANDS                              " "CYM"     136)
    ("CF"  "CENTRAL AFRICAN REPUBLIC                    " "CAF"     140)
    ("TD"  "CHAD                                        " "TCD"     148)
    ("CL"  "CHILE                                       " "CHL"     152)
    ("CN"  "CHINA                                       " "CHN"     156)
    ("CX"  "CHRISTMAS ISLAND                            " "CXR"     162)
    ("CC"  "COCOS (KEELING) ISLANDS                     " "CCK"     166)
    ("CO"  "COLOMBIA                                    " "COL"     170)
    ("KM"  "COMOROS                                     " "COM"     174)
    ("CD"  "CONGO, Democratic Republic of (was Zaire)   " "COD"     180)
    ("CG"  "CONGO, Republic of                          " "COG"     178)
    ("CK"  "COOK ISLANDS                                " "COK"     184)
    ("CR"  "COSTA RICA                                  " "CRI"     188)
    ("CI"  "COTE D'IVOIRE                               " "CIV"     384)
    ("HR"  "CROATIA (local name: Hrvatska)              " "HRV"     191)      
    ("CU"  "CUBA                                        " "CUB"     192)
    ("CY"  "CYPRUS                                      " "CYP"     196)
    ("CZ"  "CZECH REPUBLIC                              " "CZE"     203)  
    ("DK"  "DENMARK                                     " "DNK"     208)
    ("DJ"  "DJIBOUTI                                    " "DJI"     262)
    ("DM"  "DOMINICA                                    " "DMA"     212)
    ("DO"  "DOMINICAN REPUBLIC                          " "DOM"     214)
    ("EC"  "ECUADOR                                     " "ECU"     218)
    ("EG"  "EGYPT                                       " "EGY"     818)
    ("SV"  "EL SALVADOR                                 " "SLV"     222)
    ("GQ"  "EQUATORIAL GUINEA                           " "GNQ"     226)
    ("ER"  "ERITREA                                     " "ERI"     232)
    ("EE"  "ESTONIA                                     " "EST"     233)  
    ("ET"  "ETHIOPIA                                    " "ETH"     231)
    ("FK"  "FALKLAND ISLANDS (MALVINAS)                 " "FLK"     238)
    ("FO"  "FAROE ISLANDS                               " "FRO"     234)
    ("FJ"  "FIJI                                        " "FJI"     242)
    ("FI"  "FINLAND                                     " "FIN"     246)
    ("FR"  "FRANCE                                      " "FRA"     250)
    ("GF"  "FRENCH GUIANA                               " "GUF"     254)
    ("PF"  "FRENCH POLYNESIA                            " "PYF"     258)
    ("TF"  "FRENCH SOUTHERN TERRITORIES                 " "ATF"     260)
    ("GA"  "GABON                                       " "GAB"     266)
    ("GM"  "GAMBIA                                      " "GMB"     270)
    ("GE"  "GEORGIA                                     " "GEO"     268)  
    ("DE"  "GERMANY                                     " "DEU"     276)
    ("GH"  "GHANA                                       " "GHA"     288)
    ("GI"  "GIBRALTAR                                   " "GIB"     292)
    ("GR"  "GREECE                                      " "GRC"     300)
    ("GL"  "GREENLAND                                   " "GRL"     304)
    ("GD"  "GRENADA                                     " "GRD"     308)
    ("GP"  "GUADELOUPE                                  " "GLP"     312)
    ("GU"  "GUAM                                        " "GUM"     316)
    ("GT"  "GUATEMALA                                   " "GTM"     320)
    ("GN"  "GUINEA                                      " "GIN"     324)
    ("GW"  "GUINEA-BISSAU                               " "GNB"     624)
    ("GY"  "GUYANA                                      " "GUY"     328)
    ("HT"  "HAITI                                       " "HTI"     332)
    ("HM"  "HEARD AND MC DONALD ISLANDS                 " "HMD"     334)
    ("HN"  "HONDURAS                                    " "HND"     340)
    ("HK"  "HONG KONG                                   " "HKG"     344)
    ("HU"  "HUNGARY                                     " "HUN"     348)
    ("IS"  "ICELAND                                     " "ISL"     352)
    ("IN"  "INDIA                                       " "IND"     356)
    ("ID"  "INDONESIA                                   " "IDN"     360)
    ("IR"  "IRAN (ISLAMIC REPUBLIC OF)                  " "IRN"     364)
    ("IQ"  "IRAQ                                        " "IRQ"     368)
    ("IE"  "IRELAND                                     " "IRL"     372)
    ("IL"  "ISRAEL                                      " "ISR"     376)
    ("IT"  "ITALY                                       " "ITA"     380)
    ("JM"  "JAMAICA                                     " "JAM"     388)
    ("JP"  "JAPAN                                       " "JPN"     392)
    ("JO"  "JORDAN                                      " "JOR"     400)
    ("KZ"  "KAZAKHSTAN                                  " "KAZ"     398)  
    ("KE"  "KENYA                                       " "KEN"     404)
    ("KI"  "KIRIBATI                                    " "KIR"     296)
    ("KP"  "KOREA, DEMOCRATIC PEOPLE'S REPUBLIC OF      " "PRK"     408)
    ("KR"  "KOREA, REPUBLIC OF                          " "KOR"     410)
    ("KW"  "KUWAIT                                      " "KWT"     414)
    ("KG"  "KYRGYZSTAN                                  " "KGZ"     417)  
    ("LA"  "LAO PEOPLE'S DEMOCRATIC REPUBLIC            " "LAO"     418)
    ("LV"  "LATVIA                                      " "LVA"     428)  
    ("LB"  "LEBANON                                     " "LBN"     422)
    ("LS"  "LESOTHO                                     " "LSO"     426)
    ("LR"  "LIBERIA                                     " "LBR"     430)
    ("LY"  "LIBYAN ARAB JAMAHIRIYA                      " "LBY"     434)
    ("LI"  "LIECHTENSTEIN                               " "LIE"     438)
    ("LT"  "LITHUANIA                                   " "LTU"     440)  
    ("LU"  "LUXEMBOURG                                  " "LUX"     442)
    ("MO"  "MACAU                                       " "MAC"     446)
    ("MK"  "MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF  " "MKD"     807) 
    ("MG"  "MADAGASCAR                                  " "MDG"     450)
    ("MW"  "MALAWI                                      " "MWI"     454)
    ("MY"  "MALAYSIA                                    " "MYS"     458)
    ("MV"  "MALDIVES                                    " "MDV"     462)
    ("ML"  "MALI                                        " "MLI"     466)
    ("MT"  "MALTA                                       " "MLT"     470)
    ("MH"  "MARSHALL ISLANDS                            " "MHL"     584)
    ("MQ"  "MARTINIQUE                                  " "MTQ"     474)
    ("MR"  "MAURITANIA                                  " "MRT"     478)
    ("MU"  "MAURITIUS                                   " "MUS"     480)
    ("YT"  "MAYOTTE                                     " "MYT"     175)  
    ("MX"  "MEXICO                                      " "MEX"     484)
    ("FM"  "MICRONESIA, FEDERATED STATES OF             " "FSM"     583)
    ("MD"  "MOLDOVA, REPUBLIC OF                        " "MDA"     498)  
    ("MC"  "MONACO                                      " "MCO"     492)
    ("MN"  "MONGOLIA                                    " "MNG"     496)
    ("MS"  "MONTSERRAT                                  " "MSR"     500)
    ("MA"  "MOROCCO                                     " "MAR"     504)
    ("MZ"  "MOZAMBIQUE                                  " "MOZ"     508)
    ("MM"  "MYANMAR                                     " "MMR"     104)
    ("NA"  "NAMIBIA                                     " "NAM"     516)
    ("NR"  "NAURU                                       " "NRU"     520)
    ("NP"  "NEPAL                                       " "NPL"     524)
    ("NL"  "NETHERLANDS                                 " "NLD"     528)
    ("AN"  "NETHERLANDS ANTILLES                        " "ANT"     530)
    ("NC"  "NEW CALEDONIA                               " "NCL"     540)
    ("NZ"  "NEW ZEALAND                                 " "NZL"     554)
    ("NI"  "NICARAGUA                                   " "NIC"     558)
    ("NE"  "NIGER                                       " "NER"     562)
    ("NG"  "NIGERIA                                     " "NGA"     566)
    ("NU"  "NIUE                                        " "NIU"     570)
    ("NF"  "NORFOLK ISLAND                              " "NFK"     574)
    ("MP"  "NORTHERN MARIANA ISLANDS                    " "MNP"     580)
    ("NO"  "NORWAY                                      " "NOR"     578)
    ("OM"  "OMAN                                        " "OMN"     512)
    ("PK"  "PAKISTAN                                    " "PAK"     586)
    ("PW"  "PALAU                                       " "PLW"     585)
    ("PS"  "PALESTINIAN TERRITORY, Occupied             " "PSE"     275)
    ("PA"  "PANAMA                                      " "PAN"     591)
    ("PG"  "PAPUA NEW GUINEA                            " "PNG"     598)
    ("PY"  "PARAGUAY                                    " "PRY"     600)
    ("PE"  "PERU                                        " "PER"     604)
    ("PH"  "PHILIPPINES                                 " "PHL"     608)
    ("PN"  "PITCAIRN                                    " "PCN"     612)
    ("PL"  "POLAND                                      " "POL"     616)
    ("PT"  "PORTUGAL                                    " "PRT"     620)
    ("PR"  "PUERTO RICO                                 " "PRI"     630)
    ("QA"  "QATAR                                       " "QAT"     634)
    ("RE"  "REUNION                                     " "REU"     638)
    ("RO"  "ROMANIA                                     " "ROU"     642)
    ("RU"  "RUSSIAN FEDERATION                          " "RUS"     643)
    ("RW"  "RWANDA                                      " "RWA"     646)
    ("SH"  "SAINT HELENA                                " "SHN"     654)
    ("KN"  "SAINT KITTS AND NEVIS                       " "KNA"     659)
    ("LC"  "SAINT LUCIA                                 " "LCA"     662)
    ("PM"  "SAINT PIERRE AND MIQUELON                   " "SPM"     666)
    ("VC"  "SAINT VINCENT AND THE GRENADINES            " "VCT"     670)
    ("WS"  "SAMOA                                       " "WSM"     882)
    ("SM"  "SAN MARINO                                  " "SMR"     674)
    ("ST"  "SAO TOME AND PRINCIPE                       " "STP"     678)
    ("SA"  "SAUDI ARABIA                                " "SAU"     682)
    ("SN"  "SENEGAL                                     " "SEN"     686)
    ("CS"  "SERBIA AND MONTENEGRO                       " "SCG"     891)
    ("SC"  "SEYCHELLES                                  " "SYC"     690)
    ("SL"  "SIERRA LEONE                                " "SLE"     694)
    ("SG"  "SINGAPORE                                   " "SGP"     702)
    ("SK"  "SLOVAKIA                                    " "SVK"     703)  
    ("SI"  "SLOVENIA                                    " "SVN"     705)  
    ("SB"  "SOLOMON ISLANDS                             " "SLB"     090)
    ("SO"  "SOMALIA                                     " "SOM"     706)
    ("ZA"  "SOUTH AFRICA                                " "ZAF"     710)
    ("GS"  "SOUTH GEORGIA AND THE SOUTH SANDWICH ISLANDS" "SGS"     239)
    ("ES"  "SPAIN                                       " "ESP"     724)
    ("LK"  "SRI LANKA                                   " "LKA"     144)
    ("SD"  "SUDAN                                       " "SDN"     736)
    ("SR"  "SURINAME                                    " "SUR"     740)
    ("SJ"  "SVALBARD AND JAN MAYEN ISLANDS              " "SJM"     744)
    ("SZ"  "SWAZILAND                                   " "SWZ"     748)
    ("SE"  "SWEDEN                                      " "SWE"     752)
    ("CH"  "SWITZERLAND                                 " "CHE"     756)
    ("SY"  "SYRIAN ARAB REPUBLIC                        " "SYR"     760)
    ("TW"  "TAIWAN                                      " "TWN"     158)
    ("TJ"  "TAJIKISTAN                                  " "TJK"     762)  
    ("TZ"  "TANZANIA, UNITED REPUBLIC OF                " "TZA"     834)
    ("TH"  "THAILAND                                    " "THA"     764)
    ("TL"  "TIMOR-LESTE                                 " "TLS"     626)
    ("TG"  "TOGO                                        " "TGO"     768)
    ("TK"  "TOKELAU                                     " "TKL"     772)
    ("TO"  "TONGA                                       " "TON"     776)
    ("TT"  "TRINIDAD AND TOBAGO                         " "TTO"     780)
    ("TN"  "TUNISIA                                     " "TUN"     788)
    ("TR"  "TURKEY                                      " "TUR"     792)
    ("TM"  "TURKMENISTAN                                " "TKM"     795)  
    ("TC"  "TURKS AND CAICOS ISLANDS                    " "TCA"     796)
    ("TV"  "TUVALU                                      " "TUV"     798)
    ("UG"  "UGANDA                                      " "UGA"     800)
    ("UA"  "UKRAINE                                     " "UKR"     804)
    ("AE"  "UNITED ARAB EMIRATES                        " "ARE"     784)
    ("GB"  "UNITED KINGDOM                              " "GBR"     826)
    ("US"  "UNITED STATES                               " "USA"     840)
    ("UM"  "UNITED STATES MINOR OUTLYING ISLANDS        " "UMI"     581)
    ("UY"  "URUGUAY                                     " "URY"     858)
    ("UZ"  "UZBEKISTAN                                  " "UZB"     860)  
    ("VU"  "VANUATU                                     " "VUT"     548)
    ("VA"  "VATICAN CITY STATE (HOLY SEE)               " "VAT"     336)
    ("VE"  "VENEZUELA                                   " "VEN"     862)
    ("VN"  "VIET NAM                                    " "VNM"     704)
    ("VG"  "VIRGIN ISLANDS (BRITISH)                    " "VGB"     092)
    ("VI"  "VIRGIN ISLANDS (U.S.)                       " "VIR"     850)
    ("WF"  "WALLIS AND FUTUNA ISLANDS                   " "WLF"     876)
    ("EH"  "WESTERN SAHARA                              " "ESH"     732)
    ("YE"  "YEMEN                                       " "YEM"     887)
    ("ZM"  "ZAMBIA                                      " "ZMB"     894)
    ("ZW"  "ZIMBABWE                                    " "ZWE"     716)))





(defun geo-form-page (geoid &optional parameters)
  (when-let (geo (ele:get-instance-by-value 'geography
					    'geography-name
					    (url-decode geoid)))
    (when parameters
      (setf (geo-members geo)
	    (remove-if-not (lambda (p)
			     (get-country-info-from-iso-code p))
			   (mapcar #'car parameters))))
    (geo-form geo)))

(defun geo-postage-page (geo &optional parameters)
  (flet ((postage-rows (provider-name rates)
	   (with-html-output-to-string (s)
	     (dolist (rate rates)
	       (htm (:tr (:td (str (provider-entry-weight rate)))
			 (:td (str
			       (print-price (provider-entry-price rate))))
			 (:td ((:a :href (add-get-parameters-to-url
					  (get-edit-page-url geo :postage-rates)
					  (acons "delete" (provider-entry-weight rate)
						 (acons "provider" provider-name nil))) 
				   :class "btn btn-small btn-danger")
			       "X"))))))))
    

    (with-html-output-to-string (s)
      ((:form :action (get-edit-url geo)
	      :method "get"
	      :class "form-search")
       (:input :type "text" :name "newprovider")
       ((:button :type "submit" :class "btn")
	"New provider"))
      (dolist (p (geo-providers geo))
	(htm (:h2 (str (provider-name p)))
	     ((:form :action (get-edit-page-url geo :postage-rates)
		     :class "form form-search"
		     :method "get")
	      (:input :type "hidden" :name "provider"
		      :value (provider-name p))
	      (:input :type "text" :name "weight"
		      :placeholder "Weight (g)")
	      (:input :type "text" :name "price"
		      :placeholder "Price (cents)")
	      ((:button :type "submit"
			:class "btn")
	       "Add rate"))
	     ((:a :href (add-get-parameters-to-url
			 (get-edit-page-url geo :postage-rates)
			 (acons "deleteprovider" (provider-name p) nil))
		  :class "btn btn-danger pull-right")
	      "Delete provider")
	     ((:table :class "table table-striped table-bordered table-condensed")
	      (:tr (:th "Weight")
		   (:th "Price")
		   (:th "Delete"))
	      (str (postage-rows (provider-name p)
				 (get-postage-rates p)))))))))

(defun maybe-update-providers (geo &optional (parameters (get-parameters*)))
  (logger :debug "MAYBE-UPDATE-PROVIDERS - geo - ~S ; parameters - ~S" geo parameters)
  (when parameters 
    (flet ((get-parameter (p) (cdr (assoc p parameters :test #'string-equal)))
	   (get-integer (str) (ignore-errors (parse-integer str :junk-allowed t))))
      (when-let* ((provider-name (get-parameter "provider"))
		  (provider-object (find provider-name (geo-providers geo)
					 :key #'provider-name :test #'string-equal)))
	(when-let* ((provider-delete (get-parameter "delete"))
		    (delete-number (get-integer provider-delete)))
	  (setf (items provider-object)
		(remove delete-number (items provider-object)
			:key #'provider-entry-weight)))
	(when-let* ((provider-weight (get-parameter "weight"))
		    (provider-price (get-parameter "price"))
		    (weight (get-integer provider-weight))
		    (price (get-integer provider-price)))
	  (setf (items provider-object)
		(cons (list price weight)
		      (remove weight (items provider-object)
			      :key #'provider-entry-weight)))
	  (set-item-quantity price provider-object weight)))
    
      (when-let ((new-provider (get-parameter "newprovider")))
	(unless (find new-provider (geo-providers geo)
		      :key #'provider-name :test #'string-equal)
	  (push (make-instance 'provider :name new-provider) (geo-providers geo))))
    
      (when-let ((delete-provider (get-parameter "deleteprovider")))
	(setf (geo-providers geo) (remove delete-provider (geo-providers geo)
					  :key #'provider-name :test #'string-equal))))))

;; (defun geo-postage-page (geoid &optional parameters)
;;   (when-let (geo (ele:get-instance-by-value 'geography
;; 					    'geography-name
;; 					    (url-decode geoid)))
;;     (flet ((postage-rows (provider-name rates)
;; 	     (with-html-output-to-string (s)
;; 	       (dolist (rate rates)
;; 		 (htm (:tr (:td (str (provider-entry-weight rate)))
;; 			   (:td (str
;; 				 (print-price (provider-entry-price rate))))
;; 			   (:td ((:a :href (add-get-parameters-to-url
;; 					    (get-edit-page-url geo :postage-rates)
;; 					    (acons "delete" (provider-entry-weight rate)
;; 						   (acons "provider" provider-name nil))) 
;; 				     :class "btn btn-small btn-danger")
;; 				 "X")))))))
;; 	   (get-parameter (p) (cdr (assoc p parameters :test #'string-equal)))
;; 	   (get-integer (str) (ignore-errors (parse-integer str :junk-allowed t))))
;;       (when-let* ((provider-name (get-parameter "provider"))
;; 		  (provider-object (find provider-name (geo-providers geo)
;; 					 :key #'provider-name :test #'string-equal)))
;; 	(when-let* ((provider-delete (get-parameter "delete"))
;; 		    (delete-number (get-integer provider-delete)))
;; 	  (setf (items provider-object)
;; 		(remove delete-number (items provider-object)
;; 			:key #'provider-entry-weight)))
;; 	(when-let* ((provider-weight (get-parameter "weight"))
;; 		    (provider-price (get-parameter "price"))
;; 		    (weight (get-integer provider-weight))
;; 		    (price (get-integer provider-price)))
;; 	  (setf (items provider-object)
;; 		(cons (list price weight)
;; 		      (remove weight (items provider-object)
;; 			      :key #'provider-entry-weight)))
;; 	  (set-item-quantity price provider-object weight)))

;;       (when-let ((new-provider (get-parameter "newprovider")))
;; 	(unless (find new-provider (geo-providers geo)
;; 		      :key #'provider-name :test #'string-equal)
;; 	  (push (make-instance 'provider :name new-provider) (geo-providers geo))))

;;       (when-let ((delete-provider (get-parameter "deleteprovider")))
;; 	(setf (geo-providers geo) (remove delete-provider (geo-providers geo)
;; 					  :key #'provider-name :test #'string-equal)))

;;       (with-html-output-to-string (s)
;; 				((:form :action (get-edit-page-url geo :postage-rates)
;; 					:method "get"
;; 					:class "form-search")
;; 				 (:input :type "text" :name "newprovider")
;; 				 ((:button :type "submit" :class "btn")
;; 				  "New provider"))
;; 				(dolist (p (geo-providers geo))
;; 				  (htm (:h2 (str (provider-name p)))
;; 				       ((:form :action (get-edit-page-url geo :postage-rates)
;; 					       :class "form form-search"
;; 					       :method "get")
;; 					(:input :type "hidden" :name "provider"
;; 						:value (provider-name p))
;; 					(:input :type "text" :name "weight"
;; 						:placeholder "Weight (g)")
;; 					(:input :type "text" :name "price"
;; 						:placeholder "Price (cents)")
;; 					((:button :type "submit"
;; 						  :class "btn")
;; 					 "Add rate"))
;; 				       ((:a :href (add-get-parameters-to-url
;; 						    (get-edit-page-url geo :postage-rates)
;; 						    (acons "deleteprovider" (provider-name p) nil))
;; 					    :class "btn btn-danger pull-right")
;; 					"Delete provider")
;; 				       ((:table :class "table table-striped table-bordered table-condensed")
;; 					(:tr (:th "Weight")
;; 					     (:th "Price")
;; 					     (:th "Delete"))
;; 					(str (postage-rows (provider-name p)
;; 							   (get-postage-rates p))))))))))


;; (defmethod edit-tabs ((geo geography) active)
;;   (nav-tabs `((,(get-geo-edit-url geo) . "Countries")
;; 	      (,(get-geo-postage-url geo) . "Postage rates"))
;; 	    active))

;; (defun get-geo-edit-url (geo)
;;   (restas:genurl 'shopper-edit:geo/edit :geoid (get-identifier geo)))

;; (defun get-geo-postage-url (geo)
;;   (restas:genurl 'shopper-edit:geo/edit/postage
;; 		 :geoid (url-encode (geo-name geo))))

;; (defun get-geo-delete-url (geo)
;;   (restas:genurl 'shopper-edit:geo/delete
;; 		 :geoid (url-encode (geo-name geo))))



(defun geo-form (&optional geo)
  (with-html-output-to-string (s nil :indent t)
    ((:form :action (if geo
			(get-edit-url geo)
			(get-new-url :geography))
	    :method :post)
     (textfield "title" s "Name" "Name or title of this geography"
		(when geo (geo-name geo)))
     (when geo
       (htm (:p (:strong "Members: ")
		(fmt "~{~A~^, ~}."
		     (mapcar (compose #'cdr
				      #'get-country-info-from-iso-code)
			     (geo-members geo))))))
     
     (:hr)
     ((:div :class "row-fluid")
      (submit-button "Submit" s))
     ((:div :class "row-fluid")
      (dolist (part (partition-list
		     (all-countries-info)
		     (* (ceiling (length (all-countries-info)) 3))))
	(htm ((:div :class "span4")
	      (dolist (g part)
		(destructuring-bind (code . name) g
		  (checkbox code s name (when geo (country-in? code geo)))
		  (htm (:br))))))))
     ((:div :class "row-fluid")
      (submit-button "Submit" s)))))

(defun country-in? (code geo)
  (member code (geo-members geo)
	  :test #'equal))

;; FIXME
(defun item-available-in? (item geo)
  (member geo (geographies item)))




(defun postage-options (cart customer)
  (when-let (geo (get-geo-from-country-code (country customer)))
    (logger :debug "Got geo: ~S" geo)
    (when-let (rates (get-applicable-postage-rates
		      (get-weight cart)
		      geo))
      (sort rates #'< :key #'cdr))))

(defun toggle-geo (item geo)
  (if (item-available-in? item geo)
      (setf (geographies item) (remove geo (geographies item)))
      (push geo (geographies item))))
