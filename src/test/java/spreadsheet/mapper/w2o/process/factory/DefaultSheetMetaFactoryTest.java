package spreadsheet.mapper.w2o.process.factory;

import org.testng.annotations.Test;
import spreadsheet.mapper.AssertUtil;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.meta.SheetMeta;

/**
 * Created by hanwen on 2017/1/11.
 */
public class DefaultSheetMetaFactoryTest {

  @Test
  public void testCreate() throws Exception {

    SheetMeta expected = TestFactory.createSheetMeta(true);

    SheetMetaFactory sheetMetaFactory = new DefaultSheetMetaFactory(1, 2, new int[]{1});

    Sheet sheet = TestFactory.createSheet();

    SheetMeta sheetMeta = sheetMetaFactory.create(sheet);

    AssertUtil.assertSheetMetaEquals(sheetMeta, expected);
  }

}