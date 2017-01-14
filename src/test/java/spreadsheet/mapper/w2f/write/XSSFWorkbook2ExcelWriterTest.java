package spreadsheet.mapper.w2f.write;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;
import spreadsheet.mapper.AssertUtil;
import spreadsheet.mapper.Constants;
import spreadsheet.mapper.TestFactory;
import spreadsheet.mapper.f2w.read.Excel2WorkbookReader;
import spreadsheet.mapper.f2w.read.WorkbookReader;
import spreadsheet.mapper.model.core.Workbook;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

/**
 * Created by hanwen on 2017/1/5.
 */
@Test(groups = "XSSFWorkbook2ExcelWriterTest", dependsOnGroups = "Excel2WorkbookReaderTest")
public class XSSFWorkbook2ExcelWriterTest {

  private static final Logger LOGGER = LoggerFactory.getLogger(XSSFWorkbook2ExcelWriterTest.class);

  private File file;

  @BeforeClass
  public void before() throws IOException {
    file = File.createTempFile("test", Constants.SUFFIX_XLSX);
    LOGGER.debug(file.getAbsolutePath());
  }

  @Test
  public void testWrite() throws Exception {

    Workbook workbook = TestFactory.createWorkbook();

    WorkbookWriter workbookWriter = new Workbook2ExcelWriter();

    workbookWriter.write(workbook, new FileOutputStream(file));

    WorkbookReader reader = new Excel2WorkbookReader();

    Workbook workbook1 = reader.read(new FileInputStream(file));

    AssertUtil.assertWorkbookEquals(workbook1, true);
  }
}
