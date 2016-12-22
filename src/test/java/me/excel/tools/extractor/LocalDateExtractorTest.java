package me.excel.tools.extractor;

import me.excel.tools.factory.TestPersonModel;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2016/12/22.
 */
public class LocalDateExtractorTest {

  @Test
  public void testGetStringValue() throws Exception {
    TestPersonModel person = TestPersonModel.create();

    LocalDateExtractor localDateExtractor = new LocalDateExtractor("person.birthday", "yyyy-MM-dd");
    String result = localDateExtractor.getStringValue(person);

    assertEquals("1984-11-22", result);
  }

}