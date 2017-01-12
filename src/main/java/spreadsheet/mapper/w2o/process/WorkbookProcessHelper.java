package spreadsheet.mapper.w2o.process;

import java.util.List;

/**
 * workbook process helper, simply adapter of sheet process helper
 * <p>
 * Created by hanwen on 2017/1/4.
 */
public interface WorkbookProcessHelper {

  /**
   * @param sheetProcessHelpers {@link SheetProcessHelper}
   * @return {@link WorkbookProcessHelper}
   */
  WorkbookProcessHelper sheetProcesses(SheetProcessHelper... sheetProcessHelpers);

  /**
   * @return list of sheet list data
   * @see SheetProcessHelper#process()
   */
  List<List> process();
}
