package spread.sheet.model.core;

import java.io.Serializable;

/**
 * cell
 * <p>
 * Created by hanwen on 15-12-16.
 */
public interface Cell extends Serializable, Comparable<Cell> {

  /**
   * cell value
   *
   * @return value
   */
  String getValue();

  /**
   * cell column index
   *
   * @return 1-based
   */
  int getColumnIndex();

  /**
   * @return the row of this
   */
  Row getRow();
}
