import scala.util.{Try, Failure, Success}


class Livre(val titre: String, val auteur: String, val anneeDePublication: Int)
{
  private var estEmprunté: Boolean = false

  def emprunter(): Unit =
  {
    Try
    {
      if (estEmprunté) //verifie s'il est emprunté
      {
        throw new Exception(s"Le livre '$titre' est déjà emprunté.")
      }
      estEmprunté = true //sinon il le rend emprunte
      println(s"Le livre '$titre' a été emprunté.")
    }match
    { // Si l'emprunt réussit, retourne Success
      case Success(result) => Success(result)
      // Pour gérer les exceptions : afficher message d'erreur et retourner Failure
      case Failure(exception) =>
      {
        println(exception.getMessage)
        Failure(exception)
      }


    }
  }

  def rendre(titre: String): Try[Unit] =
  {
    Try
    {
      if (!estEmprunté)
      {
        throw new Exception("Le livre n'est pas emprunté.")
      } else
      {
        estEmprunté = false
        println(s"Le livre '$titre' a été rendu.")
      }
    }match
    {
      case Success(result) => Success(result)
      case Failure(exception) => {
        println(exception.getMessage)
        Failure(exception)
      }


    }
  }

  // Pour obtenir l'état d'emprunt du livre
  def getestEmprunté: Boolean = estEmprunté
}