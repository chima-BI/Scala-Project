class Bibliotheque
{
  private var livres: List[Livre] = List() //pour stocker les livres


  //ajouter un livre
  def ajouterLivre(livre: Livre): Unit =
  {
    livres = livre :: livres
    println(s"Le livre a été ajouté avec succès")
  }

  def emprunterLivre(titre: String): Unit =
  {
    livres.find(_.titre == titre) match //recherche par titre
    {
      case Some(livre) => livre.emprunter() //si trouvé, il est emprunté
      // on gere le cas ou il est déja emprunté dans la classe livre
      case None => println(s"Le livre '$titre' n'est pas dans la bibliothèque.") //s'il n'existe pas
    }
  }

  def rendreLivre(titre: String): Unit =
  {
    livres.find(_.titre == titre) match //recherche par titre
    {
      case Some(livre) => livre.rendre(livre.titre)  // Si trouvé, il est rendu
      // on gere le cas ou il est déja rendu dans la classe livre
      case None => println(s"Le livre '$titre' n'est pas dans la bibliothèque.") //s'il n'existe pas
    }
  }

  // Rechercher un livre par titre
  def rechercherParTitre(titre: String): Option[Livre] = {
    livres.find(_.titre == titre) //recherche par titre
  }
  // Par auteur
  def rechercherParAuteur(auteur: String): List[Livre] = {
    livres.filter(_.auteur == auteur)
  }

  def getLivres: List[Livre] = livres
}