package spreadsheet.mapper.utils.builder;

import org.testng.annotations.Test;
import spreadsheet.mapper.AssertUtil;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.model.core.Sheet;
import spreadsheet.mapper.model.meta.SheetMeta;

/**
 * Created by hanwen on 2017/1/11.
 */
public class SheetBasedSheetMetaBuilderTest {

  @Test
  public void testCreate() throws Exception {

    SheetMeta expected = TestFactory.createSheetMeta(true);

    Sheet sheet = TestFactory.createSheet();

    SheetBasedSheetMetaBuilder sheetMetaBuilder = new SheetBasedSheetMetaBuilder();

    SheetMeta sheetMeta = sheetMetaBuilder.sheet(sheet)
        .fieldRowIndex(1)
        .fieldPrefixes("test.")
        .dataStartRowIndex(2)
        .headerRowIndices(1)
        .toSheetMeta();

    AssertUtil.assertSheetMetaEquals(sheetMeta, expected);
  }

}