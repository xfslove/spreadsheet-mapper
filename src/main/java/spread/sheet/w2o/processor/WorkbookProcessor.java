package spread.sheet.w2o.processor;

import spread.sheet.model.core.SheetList;

import java.util.List;

/**
 * workbook processor
 * <p>
 * Created by hanwen on 2017/1/4.
 */
public interface WorkbookProcessor {

  /**
   * @param sheetProcessors sheet processor
   * @see SheetProcessor
   */
  WorkbookProcessor sheetProcessor(SheetProcessor... sheetProcessors);

  /**
   * @return list of sheet list data
   * @see SheetProcessor#process()
   */
  List<SheetList<Object>> process();
}