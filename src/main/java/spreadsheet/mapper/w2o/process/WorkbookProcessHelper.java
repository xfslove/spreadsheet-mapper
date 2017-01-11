package spreadsheet.mapper.w2o.process;

import java.util.List;

/**
 * workbook processor, simply adapter of sheet processor
 * <p>
 * Created by hanwen on 2017/1/4.
 */
public interface WorkbookProcessHelper {

  /**
   * @param sheetProcessHelpers {@link SheetProcessHelper}
   */
  WorkbookProcessHelper sheetProcessHelper(SheetProcessHelper... sheetProcessHelpers);

  /**
   * @return list of sheet list data
   * @see SheetProcessHelper#process()
   */
  List<List> process();
}
