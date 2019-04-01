#lang racket

(provide (all-defined-out))
(define ciudades '(
               ("Coruña" ("Vigo" 171) ("Valladolid" 455)) 
               ("Vigo" ("Coruña" 171) ("Valladolid" 365)) 
               ("Oviedo" ("Bilbao" 304)) 
               ("Valladolid" ("Coruña" 455) ("Vigo" 365) ("Bilbao" 280) ("Madrid" 193)) 
               ("Bilbao" ("Oviedo" 304) ("Valladolid" 280) ("Zaragoza"324) ("Madrid" 395)) 
               ("Zaragoza" ("Bilbao" 324) ("Madrid" 325) ("Barcelona" 296)) 
               ("Madrid" ("Valladolid" 193) ("Bilbao" 395) ("Zaragoza" 325) ("Jaén" 335) ("Badajoz" 403) ("Albacete" 251)) 
               ("Badajoz" ("Madrid" 403)) 
               ("Barcelona" ("Zaragoza" 296) ("Gerona" 100) ("Valencia" 349)) 
               ("Gerona" ("Barcelona" 100)) 
               ("Valencia" ("Barcelona"349) ("Murcia" 241) ("Albacete" 191)) 
               ("Murcia" ("Valencia" 241) ("Albacete" 150) ("Granada" 278)) 
               ("Albacete" ("Madrid" 251) ("Valencia" 191) ("Murcia" 150)) 
               ("Jaén" ("Madrid" 335) ("Granada" 99) ("Sevilla" 242)) 
               ("Granada" ("Murcia" 278) ("Jaén" 99) ("Sevilla" 256)) 
               ("Sevilla" ("Jaén" 242) ("Granada" 256) ("Cádiz" 125)) 
               ("Cádiz" ("Sevilla" 125)) 
              )
  )

