import java.io.{File, PrintWriter}  // Pour la gestion de fichiers
import scala.io.{Source, StdIn}  // Pour la gestion des entrées/sorties
import scala.util.{Success, Try}  // Pour la gestion des erreurs

object Interaction
{
  def main(args: Array[String]): Unit =
  {
    // Charge la bibliothèque à partir du fichier de sauvegarde de la biblio, ou crée un nouveau fichier si c'est la prmière instance et que le fichier n'existe pas
    //créer une instance de la classe bibliotheque
    val bibliotheque = chargerBibliothèque("bibliotheque.txt").getOrElse(new Bibliotheque)

    var continuer = true // Pour rester dans la boucle et renvoyer le menu après chque choix sauf pour le dernier choix de quitter

    while (continuer)
    {
      println("   Menu:")
      println("   1- Ajouter un livre ")
      println("   2- Emprunter un livre")
      println("   3- Rendre un livre")
      println("   4- Rechercher par titre")
      println("   5- Rechercher par auteur")
      println("   6- Sauvegarder la bibliothèque")
      println("   7- Quitter")

      val input = StdIn.readLine() // Récupère le choix de l'utilisateur et lit la ligne d'entree
      val choix = Try(input.toInt) // impose a l'utilisateur de saisir un entier soit Success ou Failure
      choix match // utilisé pour le pattern matching et test la valeur de l'objet Try
      {
        case Success(1) =>  //si l'objet Try est une success contenant le chiffre 1
          //ajout d'un livre
          println("Entrez le titre du livre :")
          val titre = StdIn.readLine() //donner comme entree le nom du livre
          println("Entrez l'auteur du livre :")
          val auteur = StdIn.readLine() //donner comme entree l'auteur du livre
          println("Entrez l'année de publication du livre :")
          val année = StdIn.readInt() //donner comme entree la date de publication du livre
          val livre = new Livre(titre, auteur, année)  //creer une nouvelle instance de la classe Livre
          bibliotheque.ajouterLivre(livre)

        case Success(2) =>  //emprunter le livre
          println("Entrez le titre du livre à emprunter :")
          val titre = StdIn.readLine()
          bibliotheque.emprunterLivre(titre)

        case Success(3) =>  //rendre le livre
          println("Entrez le titre du livre à rendre :")
          val titre = StdIn.readLine()
          bibliotheque.rendreLivre(titre)

        case Success(4) => //rechercher par titre
          println("Entrez le titre du livre à rechercher :")
          val titreRecherche = StdIn.readLine() //comme entree le titre
          bibliotheque.rechercherParTitre(titreRecherche)  //retourne  l'objet Option[Livre]
          match
          {
            case Some(livre) =>  //extrait le livre trouvé à partir de l'objet Option[Livre]
              println(s"Le livre '$titreRecherche' a été trouvé : ${livre.titre}, ${livre.auteur}, ${livre.anneeDePublication}")
            case None => // aucun livre n'a été trouvé
              println(s"Aucun livre trouvé avec le titre '$titreRecherche'.")
          }

        case Success(5) => //rechercher par auteur
          // Rechercher par auteur
          println("Entrez le nom de l'auteur à rechercher :")
          val auteurRecherche = StdIn.readLine()
          val livresAuteur = bibliotheque.rechercherParAuteur(auteurRecherche)
          if (livresAuteur.nonEmpty)
          {
            println(s"Livres de l'auteur '$auteurRecherche' trouvés :")
            livresAuteur.foreach(livre => println(s"${livre.titre}, ${livre.anneeDePublication}"))
          } else
          {
            println(s"Aucun livre trouvé pour l'auteur '$auteurRecherche'.")
          }
        case Success(6) =>

          sauvegarderBibliotheque(bibliotheque, "bibliotheque.txt")
          println("Bibliothèque sauvegardée.")
        case Success(7)=>
          continuer = false

        case _ =>   // joker qui correspond à n'importe quelle valeur
          println("Erreur d'entree : Veuillez entrer un Entier")
      }
    }
  }

  //SAUVEGARDER LA BIBLIOTHEQUE
  def sauvegarderBibliotheque(bibliotheque: Bibliotheque, cheminFichier: String): Unit =
  {
    val file = new File(cheminFichier) //créer fichier pour écriture
    val writer = new PrintWriter(file)
    val listeLivres = bibliotheque.getLivres //la liste de tous les livres

    listeLivres.foreach  //iterer pour chaque livre
    { livre =>
      val estEmprunte = livre.getestEmprunté
      writer.write(s"${livre.titre},${livre.auteur},${livre.anneeDePublication},$estEmprunte\n")
    }

    writer.close()
  }

  //CHARGER BIBLIOTHEQUE
  def chargerBibliothèque(cheminFichier: String): Option[Bibliotheque] =
  {
    // Vérifie si le fichier de sauvegarde de la bibliotheque existe
    if (new File(cheminFichier).exists())
    {
      //ouvre fichier pour lecture
      val bufferedSource = Source.fromFile(cheminFichier)
      val lignes = bufferedSource.getLines().toList //lit les lignes
      bufferedSource.close() //fermer le fichier

      val bibliotheque = new Bibliotheque
      //itérer sur les lignes
      for (ligne <- lignes)
      {
        // Comme fichier csv : Séparation en utilisant la virgule comme délimiteur

        val infos = ligne.split(",")
        if (infos.length == 4) //le nombre d'info sur les livres
        {
          val titre = infos(0)
          val auteur = infos(1)
          val anneeDePublication = infos(2).toInt
          val estEmprunte = infos(3).toBoolean
          val livre = new Livre(titre, auteur, anneeDePublication)
          if (estEmprunte)
          {
            livre.emprunter()
          }
          bibliotheque.ajouterLivre(livre)
        }
      }

      Some(bibliotheque)
    } else
    {
      None
    }
  }
}