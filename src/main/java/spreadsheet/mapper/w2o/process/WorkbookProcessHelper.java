package spreadsheet.mapper.w2o.process;

import java.util.List;

/**
 * workbook process helper, simply adapter of sheet process helper
 * <p>
 * Created by hanwen on 2017/1/4.
 */
public interface WorkbookProcessHelper {

  /**
   * @param sheetProcessHelper {@link SheetProcessHelper}
   * @return {@link WorkbookProcessHelper}
   */
  WorkbookProcessHelper addSheetProcessHelper(SheetProcessHelper sheetProcessHelper);

  /**
   * @return list of sheet list data
   * @see SheetProcessHelper#process()
   */
  List<List> process();
}
