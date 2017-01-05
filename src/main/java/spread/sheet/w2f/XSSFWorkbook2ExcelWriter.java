package spread.sheet.w2f;

import spread.sheet.model.core.Workbook;
import org.apache.poi.xssf.streaming.SXSSFWorkbook;

/**
 * * workbook to excel writer use {@link SXSSFWorkbook}
 * <p>
 * Created by hanwen on 2016/12/30.
 */
public class XSSFWorkbook2ExcelWriter extends Workbook2ExcelWriterAdapter {

  @Override
  protected org.apache.poi.ss.usermodel.Workbook createWorkbook(Workbook workbook) {
    // keep 100 rows in memory, exceeding rows will be flushed to disk
    return new SXSSFWorkbook(100);
  }

  @Override
  protected void afterWrite(org.apache.poi.ss.usermodel.Workbook workbook) {
    ((SXSSFWorkbook) workbook).dispose();
  }
}
