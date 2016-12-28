package me.excel.tools.model.template;

import java.io.Serializable;

/**
 * <pre>
 * sheet header include:
 * 1. which row is field row
 * when read must has field row, this is importance when read excel, is determined cell value corresponding which field of object
 * when write may empty
 * 2. which row is title (may empty)
 * 3. which row is prompt (may empty)
 * 4. data start at which row (must be after title, field and prompt rows if has)
 *
 * default sheet header format:
 *
 * first  row : titles
 * --------------------
 * second row : fields
 * --------------------
 * third  row : prompts
 * --------------------
 * data   row
 * ...
 * </pre>
 * Created by hanwen on 2016/12/27.
 */
public interface SheetHeader extends Serializable {

  /**
   * which sheet header
   *
   * @return 1-based
   */
  int getSheetIndex();

  /**
   * data row start at index
   *
   * @return 1-based
   */
  int getDataStartRowIndex();

  /**
   * if this get false, {@link #getFieldRowIndex()} will ignore when read header even if workbook has field row
   *
   * @return true if has field
   */
  boolean isHasField();

  /**
   * field row index
   *
   * @return 1-based
   */
  int getFieldRowIndex();

  /**
   * if this get false, {@link #getTitleRowIndex()} will ignore when read header even if workbook has title row
   *
   * @return true if has
   */
  boolean isHasTitle();

  /**
   * title row index
   *
   * @return 1-based
   */
  int getTitleRowIndex();

  /**
   * if this get false, {@link #getPromptRowIndex()} will ignore when read header even if workbook has prompt row
   *
   * @return true if has
   */
  boolean isHasPrompt();

  /**
   * prompt row index
   *
   * @return 1-based
   */
  int getPromptRowIndex();
}
