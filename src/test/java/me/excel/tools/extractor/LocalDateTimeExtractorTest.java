package me.excel.tools.extractor;

import me.excel.tools.factory.TestPersonModel;
import org.joda.time.LocalDateTime;
import org.testng.annotations.Test;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2016/12/22.
 */
public class LocalDateTimeExtractorTest {

  @Test
  public void testGetStringValue() throws Exception {
    TestPersonModel person = TestPersonModel.create();

    LocalDateTimeExtractor localDateTimeExtractor = new LocalDateTimeExtractor("person.registerTime", "yyyy-MM-dd HH:mm:ss");
    String result = localDateTimeExtractor.getStringValue(person);

    assertEquals("2000-01-01 00:00:00", result);
  }

}