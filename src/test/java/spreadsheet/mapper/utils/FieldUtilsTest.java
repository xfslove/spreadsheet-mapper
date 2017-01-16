package spreadsheet.mapper.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.annotations.Test;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.model.meta.FieldMetaBean;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;

/**
 * Created by hanwen on 2017/1/6.
 */
public class FieldUtilsTest {

  private static final Logger LOGGER = LoggerFactory.getLogger(FieldUtilsTest.class);

  @Test
  public void testSubtractBusinessKey() throws Exception {

    boolean assert_ = false;
    try {
      FieldUtils.subtractBusinessKey("test.int1");
    } catch (IllegalArgumentException e) {
      LOGGER.debug("\'test.int1\' not business key");
      if (e.getMessage().contains("business key")) {
        assert_ = true;
      }
    }

    assertTrue(assert_);

    String s = FieldUtils.subtractBusinessKey(FieldUtils.BUSINESS_KEY_PREFIX + "test.int1");
    assertEquals(s, "test.int1");

  }

  @Test
  public void testDetectRealField() throws Exception {

    FieldMeta fieldMeta1 = new FieldMetaBean("test.", "test.int1", 1);
    FieldMeta fieldMeta2 = new FieldMetaBean("test.test.", "test.test.int1", 1);

    assertEquals(FieldUtils.detectRealFieldName(fieldMeta1), "int1");
    assertEquals(FieldUtils.detectRealFieldName(fieldMeta2), "int1");
  }

}