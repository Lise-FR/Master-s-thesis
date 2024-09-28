import requests
from bs4 import BeautifulSoup
import csv

# URL du site sur lequel se trouvent tous les départements
super_url = "https://www.mon-maire.fr/maires-departements"
# On fait la requête vers le site et on attend la réponse
super_page = requests.get(super_url)

# Une fois qu'on a la réponse, on récupère le contenu de la page reçue
super_soup = BeautifulSoup(super_page.content, 'html.parser')
# Et on liste les départements contenus dans la page
liste_departements = super_soup.find("div", {"class": "list-group"})

# Pour ne scrapper que les départements après un certain nombre, si on a déjà les premiers départements dans la BDD par exemple
start = 1   # C'est start qu'il faut changer : si on ne veut pas scrapper le premier département, mettre start à 2
index = 1   # /!\ Ne pas changer

# On parcourt la liste des départements de France
for departement in liste_departements.children:
    if (index >= start):
        print(departement.text)
        ##############################
        #####  Visiter le site   #####
        ##############################
        # URL du site sur lequel se trouvent tous les maires du département
        url = departement.a["href"]
        page = requests.get(url)

        soup = BeautifulSoup(page.content, 'html.parser')

        ##############################
        ##### Lister les maires  #####
        ##############################
        maires_list = soup.find("ul", {"class": "list-group"})

        data = []

        # On parcourt la liste des maires du département
        for maire in maires_list.children:
            ##############################
            #####  Séparer la ville  #####
            #####   et les maires    #####
            ##############################
            ville, nom = maire.text.split(" -  ")
            print(nom)

            ###################################
            #####    Aller sur le site    #####
            ##### contenant l'adresse mail#####
            ###################################
            # URL du site sur lequel se trouvent les détails du maire, donc son adresse mail
            mail_url = maire.a["href"]
            mail_page = requests.get(mail_url)
            mail_soup = BeautifulSoup(mail_page.content, 'html.parser')

            # On récupère le mail dans la page
            mail = mail_soup.find("span", {"itemprop":"email"})

            # Si l'adresse mail est présente dans la page, alors la variable "mail" a pour valeur cette adresse mail
            if mail:
                mail = mail.text

            # Sinon, la variable "mail" ne contient rien et il faut signifier qu'on n'a pas trouvé l'adresse mail
            else:
                # Afficher un message d'erreur dans la console au moment du scrapping
                print(f"La requête sur le maire {nom} ({ville}) n'a pas aboutti, ou ne contient pas de mail dans le format attendu.")

                # Mettre N/A dans la base de données pour signifier qu'on n'a pas trouvé le mail
                mail = "N/A"

                # Sauvegarder le maire dans le fichier "maires_sans_mail.txt", au cas où on ait besoin de cette liste plus tard
                with open("maires_sans_mail.txt", "a") as file:
                    file.write(ville + " " + nom + "\n")

            # Dans les deux cas (mail trouvé ou non), on met les données du maire dans la liste des maires du département
            data.append([ville.strip(), nom.strip(), mail.strip()]) # La méthode strip() permet d'enlever les espaces au début et à la fin de la chaine de caractères


        ##############################
        ##### Mettre les données #####
        #####    dans le CSV     #####
        ##############################
        with open(f"maires.csv", "a") as csv_file:
            writer = csv.writer(csv_file)

            # Si on est en train de mettre le premier département, alors il faut mettre l'en-tête car le fichier CSV est vide
            if index == 1:
                # Écriture de l'en-tête
                writer.writerow(['Ville', 'Nom', 'Mail'])

            # Écriture des données à la suite des données précédentes (à ce stade la variable "data" est la liste des informations de tous les maires du département)
            writer.writerows(data)

    # On passe au département suivant
    index += 1
