package spreadsheet.mapper.w2o.processor;

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
  WorkbookProcessHelper sheetProcessor(SheetProcessHelper... sheetProcessHelpers);

  /**
   * @return list of sheet list data
   * @see SheetProcessHelper#process()
   */
  List<List> process();
}
