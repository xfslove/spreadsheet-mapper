package spread.sheet.w2f;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import spread.sheet.AssertUtil;
import spread.sheet.Constants;
import spread.sheet.TestFactory;
import spread.sheet.f2w.Excel2WorkbookReader;
import spread.sheet.f2w.WorkbookReader;
import spread.sheet.model.core.Workbook;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

/**
 * Created by hanwen on 2017/1/5.
 */
@Test(groups = "HSSFWorkbook2ExcelWriterTest", dependsOnGroups = "Excel2WorkbookReaderTest")
public class HSSFWorkbook2ExcelWriterTest {

  private static final Logger LOGGER = LoggerFactory.getLogger(HSSFWorkbook2ExcelWriterTest.class);

  private File file;

  @BeforeClass
  public void before() throws IOException {
    file = File.createTempFile("test", Constants.SUFFIX_XLS);
    LOGGER.debug(file.getAbsolutePath());
  }

  @Test
  public void testWrite() throws Exception {

    Workbook workbook = TestFactory.createWorkbook();

    WorkbookWriter workbookWriter = new HSSFWorkbook2ExcelWriter();

    workbookWriter.write(workbook, new FileOutputStream(file));

    WorkbookReader reader = new Excel2WorkbookReader();

    Workbook workbook1 = reader.read(new FileInputStream(file));

    AssertUtil.assertWorkbookEquals(workbook1, true);
  }

}