package spreadsheet.mapper.utils;

import org.testng.annotations.Test;
import spreadsheet.mapper.model.meta.FieldMeta;
import spreadsheet.mapper.model.meta.FieldMetaBean;

import static org.testng.Assert.*;

/**
 * Created by hanwen on 2017/1/6.
 */
public class FieldUtilsTest {

  @Test
  public void testDetectRealField() throws Exception {

    FieldMeta fieldMeta1 = new FieldMetaBean("test.", "test.int1", 1);
    FieldMeta fieldMeta2 = new FieldMetaBean("test.test.", "test.test.int1", 1);

    assertEquals(FieldUtils.detectRealFieldName(fieldMeta1), "int1");
    assertEquals(FieldUtils.detectRealFieldName(fieldMeta2), "int1");
  }

}