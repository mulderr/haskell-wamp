(function () {

var connection = new autobahn.Connection({
  url: 'ws://127.0.0.1:3000/',
  realm: 'realm1'
});

connection.onopen = function (session) {
  // PubSub test
  function onevent(args) {
    console.log("received event:", args[0]);
  }

  var sub1;
  session.subscribe('com.myapp.hello', onevent)
    .then(function (res) {
      sub1 = res;
      console.log('subscribed');
      return session.publish('com.myapp.hello', ['Hello, world!'], {}, {acknowledge: true});
    }).then(function (res) {
      console.log('published');
      return session.unsubscribe(sub1);
    }).then(function (res) {
      console.log('unsubsribed');
      return session.publish('com.myapp.hello', ['Hello, world!'], {}, {acknowledge: true});
    }, function (err) {
      console.log(err);
    });
  
  // RPC test
  function add2(args) {
    return args[0] + args[1];
  }

  session.register('com.myapp.add2', add2)
    .then(function (res) {
      console.log('registration ok');
      return session.unregister(res);
    }).then(function (res) {
      console.log('unregister ok');
      return session.register('com.myapp.add2', add2);
    }).then(function (res) {
      console.log('second registration ok');
      return session.call('com.myapp.add2', [2, 3]);
    }).then(function (res) {
      console.log('Result: ' + res)
    }, function (err) {
      console.log(err);
    });

  setTimeout(function (conn) {
    conn.close();
  }, 5000, connection);
};

connection.open();

})();
