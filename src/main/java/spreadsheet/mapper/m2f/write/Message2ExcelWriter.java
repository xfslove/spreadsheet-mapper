package spreadsheet.mapper.m2f.write;

import org.apache.commons.lang3.exception.ExceptionUtils;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.usermodel.WorkbookFactory;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spreadsheet.mapper.f2w.read.WorkbookReadException;
import spreadsheet.mapper.m2f.write.strategy.MessageWriteStrategy;
import spreadsheet.mapper.m2f.write.strategy.SingleCommentInCellStrategy;
import spreadsheet.mapper.m2f.write.strategy.SingleTextBoxInSheetStrategy;
import spreadsheet.mapper.model.msg.Message;
import spreadsheet.mapper.w2f.write.WorkbookWriteException;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;

/**
 * message to excel writer decorator
 * <p>
 * Created by hanwen on 2017/1/3.
 */
public class Message2ExcelWriter implements MessageWriter {

  private static final Logger LOGGER = LoggerFactory.getLogger(Message2ExcelWriter.class);

  private Map<String, MessageWriteStrategy> strategy2writeStrategy = new HashMap<>();

  private Workbook workbook;

  {
    messageWriteStrategies(
        new SingleCommentInCellStrategy(),
        new SingleTextBoxInSheetStrategy());
  }

  /**
   * this will create a new excel workbook to write messages
   *
   * @param xlsx true use {@link XSSFWorkbook} else use {@link HSSFWorkbook}
   */
  public Message2ExcelWriter(boolean xlsx) {
    workbook = xlsx ? new XSSFWorkbook() : new HSSFWorkbook();
  }

  /**
   * this will copy a excel workbook from supplied input stream to write messages
   *
   * @param inputStream auto close
   */
  public Message2ExcelWriter(InputStream inputStream) {
    try {
      workbook = WorkbookFactory.create(inputStream);
    } catch (Exception e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookReadException(e);
    }
  }

  @Override
  public MessageWriter messageWriteStrategies(MessageWriteStrategy... messageWriteStrategies) {
    if (messageWriteStrategies == null) {
      return this;
    }
    for (MessageWriteStrategy messageWriteStrategy : messageWriteStrategies) {
      strategy2writeStrategy.put(messageWriteStrategy.getStrategy(), messageWriteStrategy);
    }
    return this;
  }

  @Override
  public void write(Collection<Message> messages, OutputStream outputStream) {
    Map<String, Collection<Message>> messageWriteStrategyMap = buildMessageWriteStrategyMap(messages);
    for (String writeStrategy : messageWriteStrategyMap.keySet()) {
      MessageWriteStrategy messageWriteStrategy = strategy2writeStrategy.get(writeStrategy);
      if (messageWriteStrategy == null) {
        throw new WorkbookWriteException("no message write helper of [" + writeStrategy + "]");
      }

      messageWriteStrategy.write(workbook, messageWriteStrategyMap.get(writeStrategy));
    }

    try {
      workbook.write(outputStream);
    } catch (IOException e) {
      LOGGER.error(ExceptionUtils.getStackTrace(e));
      throw new WorkbookWriteException(e);
    } finally {

      try {
        workbook.close();
      } catch (IOException e) {
        LOGGER.error(ExceptionUtils.getStackTrace(e));
      }
    }
  }

  private Map<String, Collection<Message>> buildMessageWriteStrategyMap(Collection<Message> messages) {
    Map<String, Collection<Message>> messageWriteStrategyMap = new HashMap<>();

    for (Message message : messages) {
      String messageWriteStrategy = message.getMessageWriteStrategy();

      if (!messageWriteStrategyMap.containsKey(messageWriteStrategy)) {
        messageWriteStrategyMap.put(messageWriteStrategy, new ArrayList<Message>());
      }
      messageWriteStrategyMap.get(messageWriteStrategy).add(message);
    }

    return messageWriteStrategyMap;
  }
}
