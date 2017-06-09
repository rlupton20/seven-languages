// Function to obtain total length of a list of strings
def totalStringLength(list: List[String]): Int =
  list.foldLeft(0){ (n,string) => n + string.length }

// Test the above function
val testList = List("These", "are", "some", "words")
val total = totalStringLength(testList)
