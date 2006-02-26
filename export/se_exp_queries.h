const char load_docs_query[] = "declare option output \"indent=no\"; \
                                let $reg-docs:= for $i in document(\"$documents.xml\")/*/SA_DOCUMENT \
											    where $i/@name != \"db_security_data\" \
												   return string-value($i/@name), \
                                    $col-docs:= for $i in document(\"$documents.xml\")/*/COLLECTION_DOCS \
                                               for $j in $i/DOCUMENT \
                                                   return fn:concat($j/@name,\"\"\" \"\"\",$i/@name) \
                                return ($reg-docs,$col-docs)";
 

const char exp_docs_query[] = "let $reg-docs:= for $i in document(\"$documents.xml\")/*/SA_DOCUMENT \
											   where $i/@name != \"db_security_data\" \
                                                 return fn:concat(\"document('\",$i/@name,\"')\"), \
                                   $col-docs:= for $i in document(\"$documents.xml\")/*/COLLECTION_DOCS \
                                               for $j in $i/DOCUMENT \
                                                 return fn:concat(\"document('\",$j/@name,\"','\",$i/@name,\"')\") \
                               return ($reg-docs,$col-docs)";

const char create_colls_query[] = "for $i in document(\"$collections.xml\")/*/COLLECTION \
								   return fn:concat(\"CREATE COLLECTION '\",$i/@name,\"'\")";

const char create_sec_query[] = "for $i in document(\"$collections.xml\")/NODATA \
								 return fn:concat(\"CREATE COLLECTION '\",$i/@name,\"'\")";

const char create_indexes_query[] = "for $i in document(\"$indexes.xml\")/INDEXES/INDEX \
                                     return \
                                       fn:concat(\"CREATE INDEX '\", $i/@title, \"' ON \", \
                                       fn:concat($i/@indexed_object,\"('\",$i/@object_title,\"')\", \"/\", $i/@value_path), \
                                       \" BY \", \
									   fn:concat($i/@indexed_object,\"('\",$i/@object_title,\"')\", \"/\", $i/@key_path), \
									   \" AS \",  $i/@key_type)";

const char load_docs_query__[] = "let $reg-docs:= for $i in document(\"$documents.xml\")/*/SA_DOCUMENT \
								                  where $i/@name != \"db_security_data\" \
											      return fn:concat(\"'\",$i/@name,\"'\"), \
                                      $col-docs:= for $i in document(\"$documents.xml\")/*/COLLECTION_DOCS \
                                                  for $j in $i/DOCUMENT \
                                                     return fn:concat(\"'\",$j/@name,\"' '\",$i/@name,\"'\"), \
                                      $all-docs:= ($reg-docs,$col-docs) \
                                 for $i in 1 to count($all-docs) \
                                 let $k := $all-docs[position()=$i] \
                                 return \
                                     fn:concat(\"LOAD '\",$i,\".xml' \",$k) ";
                                     
const char check_db_empty_query[] = "let $docs := document(\"$documents.xml\")/DOCUMENTS/SA_DOCUMENT[ @name != \"db_security_data\"] \
                                     let $cols := document(\"$collections.xml\")/COLLECTION/* \
                                     let $ind  := document(\"$indexes.xml\")/INDEXES/* \
                                     let $sec-users  := document(\"db_security_data\")/db_security_data/users/user[@user_name != \"SYSTEM\"] \
                                     let $sec-roles  := document(\"db_security_data\")/db_security_data/roles/role[@role_name != \"DBA\" and @role_name != \"PUBLIC\"] \
                                     let $all := ($docs, $cols, $ind, $sec-users, $sec-roles) \
                                     return if (empty($all)) then 1 else 0 "; 