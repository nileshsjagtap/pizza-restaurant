package app

import Domain.{CustomerOrderParser, Customers, CustomersCountParser, InputParser, InputReader}


object Run extends App {

   def main1(args: Array[String]) = {

    def run(inputFilePath: String, outputFilePath: String) = {

      val countParser: InputParser = new CustomersCountParser(dsfsdf)

      for {
        lines <- InputReader.read(inputFilePath)

        customerCount <- CustomersCountParser.parse(lines)
        customerOrders <- CustomerOrderParser.parse(lines)

        customers <- Customers(customerOrders)


      } yield customers.avgWaitingTime

      run(args(0), args(1))

    }

  }

  println("op--"+main1(Array("test/Resources/Input.txt", "")))

}
