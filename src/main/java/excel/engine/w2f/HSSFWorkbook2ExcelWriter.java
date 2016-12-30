package excel.engine.w2f;

import excel.engine.model.core.Workbook;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;

/**
 * workbook to excel writer use {@link HSSFWorkbook}
 * <p>
 * Created by hanwen on 15-12-16.
 */
public class HSSFWorkbook2ExcelWriter extends Workbook2ExcelWriterAdapter {

  @Override
  protected org.apache.poi.ss.usermodel.Workbook createWorkbook(Workbook workbook) {
    return new HSSFWorkbook();
  }

}
