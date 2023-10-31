import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.Failure

class LivreTest extends AnyFlatSpec with Matchers {
  "Livre" should "être emprunté avec succès" in {
    val livre = new Livre("Livr", "Auteur Test", 2022)
    livre.emprunter()
    livre.getestEmprunté should be(true)
  }

  it should "gérer l'emprunt d'un livre déjà emprunté" in {
    val livre = new Livre("davinci", "dan brown", 1567)
    livre.emprunter()
    val result = livre.emprunter()
    result shouldBe a [Failure[_]]
  }

  it should "être rendu avec succès" in {
    val livre = new Livre("Livre Rendable", "Auteur Test", 2022)
    livre.emprunter()
    livre.rendre("Titre")
    livre.getestEmprunté should be(false)
  }

  it should "gérer le retour d'un livre non emprunté" in {
    val livre = new Livre("Livre Non Emprunté", "Auteur Test", 2022)
    val result = livre.rendre("Livre Non Emprunté")
    result shouldBe a [Failure[_]]
  }
}